//! Falcon microprocessor abstractions.

use faucon_asm::{disassembler, Instruction};

use crate::dma;
use crate::memory::{LookupError, Memory, PageFlag};

use instructions::process_instruction;
pub use registers::*;

mod instructions;
mod registers;

/// Representation of the Falcon processor.
pub struct Cpu {
    /// The Falcon CPU registers.
    pub registers: CpuRegisters,
    /// The Falcon SRAM for code and data.
    memory: Memory,
    /// The Falcon DMA engine.
    dma_engine: dma::Engine,
    /// A boolean that is used to determine whether the PC should be
    /// incremented after an instruction.
    ///
    /// Traditionally, the PC is incremented by the instruction length
    /// after execution. But in special cases where instructions modify
    /// the PC in a custom manner, this behavior might result in invalid
    /// PC values. Thus, instructions can modify this boolean to decide
    /// whether the PC should be regularly incremented or to indicate that
    /// the instruction itself does that.
    increment_pc: bool,
}

enum_from_primitive! {
    /// Falcon trap kinds that can be delivered to the microprocessor.
    ///
    /// Traps behave similar to interrupts, but generally are triggered by
    /// internal events rather than host machine mechanisms.
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(u8)]
    pub enum Trap {
        /// A software trap that can be triggered by a TRAP instruction.
        Software0 = 0x0,
        /// A software trap that can be triggered by a TRAP instruction.
        Software1 = 0x1,
        /// A software trap that can be triggered by a TRAP instruction.
        Software2 = 0x2,
        /// A software trap that can be triggered by a TRAP instruction.
        Software3 = 0x3,
        /// A trap that is triggered whenever the processor encounters an unknown
        /// or invalid opcode.
        InvalidOpcode = 0x8,
        /// A trap that is triggered on page faults because of no hits in the TLB.
        VmNoHit = 0xA,
        /// A trap that is triggered on page faults because of multiple hits in the TLB.
        VmMultiHit = 0xB,
        /// A trap that is triggered whenever a debugging breakpoint is reached.
        Breakpoint = 0xF,
    }
}

impl Cpu {
    /// Creates a new instance of the CPU.
    pub fn new() -> Self {
        Cpu {
            registers: CpuRegisters::new(),
            memory: Memory::new(),
            dma_engine: dma::Engine::new(),
            increment_pc: false,
        }
    }

    /// Returns the length of the Falcon code segment.
    pub fn imem_size(&self) -> usize {
        self.memory.code.len()
    }

    /// Returns the length of the Falcon data segment.
    pub fn dmem_size(&self) -> usize {
        self.memory.data.len()
    }

    /// Pushes a word onto the stack and decrements the stack pointer by 4.
    pub fn stack_push(&mut self, word: u32) {
        self.registers[SP] -= 4;
        self.memory.write_data_word(self.registers[SP], word);
    }

    /// Pops a word off the stack and increments the stack pointer by 4.
    pub fn stack_pop(&mut self) -> u32 {
        let word = self.memory.read_data_word(self.registers[SP]);
        self.registers[SP] += 4;

        word
    }

    /// Triggers a [`Trap`] that should be delivered to the processor.
    ///
    /// [`Trap`]: enum.Trap.html
    pub fn trigger_trap(&mut self, trap: Trap) {
        // Set the Trap Active bit in the flags register.
        self.registers[FLAGS] |= 1 << 24;

        // Store the trap status, composed of the current PC and the trap reason.
        self.registers[TSTATUS] = self.registers[PC] | ((trap as u8 & 0xF) as u32) << 20;

        // Store the interrupt state.
        self.registers
            .set_flag(CpuFlag::IS0, self.registers.get_flag(CpuFlag::IE0));
        self.registers
            .set_flag(CpuFlag::IS1, self.registers.get_flag(CpuFlag::IE1));
        self.registers
            .set_flag(CpuFlag::IS2, self.registers.get_flag(CpuFlag::IE2));
        self.registers.set_flag(CpuFlag::IE0, false);
        self.registers.set_flag(CpuFlag::IE1, false);
        self.registers.set_flag(CpuFlag::IE2, false);

        // Push the return address onto the stack.
        self.stack_push(self.registers[PC]);

        // Jump into the trap vector.
        self.registers[PC] = self.registers[TV];
    }

    /// Uploads a code word to IMEM at a given physical and virtual address.
    pub fn upload_code(&mut self, address: u16, vaddress: u32, value: u32) {
        // TODO: Add support for all the secret stuff.
        // TODO: Nicer way to access TLB without making the borrow checker scream?

        // If the first word is being uploaded, map the page.
        if (address & 0xFC) == 0 {
            self.memory
                .tlb
                .get_physical_entry(address)
                .map(vaddress, false);
        }

        // Write word to the code segment.
        self.memory.write_code_addr(address, value);

        // If the last word was uploaded, set the Usable flag.
        if (address & 0xFC) == 0xFC {
            self.memory
                .tlb
                .get_physical_entry(address)
                .set_flag(PageFlag::Busy, false);
            self.memory
                .tlb
                .get_physical_entry(address)
                .set_flag(PageFlag::Usable, true);
        }
    }

    fn fetch_insn(&mut self, address: u32) -> Option<Instruction> {
        // Look up the TLB to get the physical code page.
        let result = match self.memory.tlb.lookup(address) {
            Ok((page, tlb)) => Some((page, tlb)),
            Err(LookupError::NoPageHits) => {
                self.trigger_trap(Trap::VmNoHit);

                None
            }
            Err(LookupError::MultiplePageHits) => {
                self.trigger_trap(Trap::VmMultiHit);

                None
            }
        };

        if let Some((page_index, tlb)) = result {
            // Build the physical code address to read from.
            let page_offset = (address & 0xFF) as u16;
            let code_address = ((page_index as u16) << 8) | page_offset;

            // If the page is marked usable, complete the access using the physical page.
            if tlb.get_flag(PageFlag::Usable) {
                let mut code = &self.memory.code[code_address as usize..];
                match disassembler::read_instruction(&mut code) {
                    Ok(insn) => Some(insn),
                    Err(faucon_asm::Error::UnknownInstruction(_)) => {
                        self.trigger_trap(Trap::InvalidOpcode);

                        None
                    }
                    Err(faucon_asm::Error::IoError) => panic!("Rust exploded"),
                    Err(faucon_asm::Error::Eof) => None,
                }
            } else if tlb.get_flag(PageFlag::Busy) {
                // The page is marked busy, the access must be completed when possible.
                todo!("Wait for the page to be marked as usable");
            } else {
                unreachable!()
            }
        } else {
            None
        }
    }

    /// Executes the next instruction at the address held by the PC register.
    pub fn step(&mut self) {
        match self.fetch_insn(self.registers[PC]) {
            Some(insn) => {
                process_instruction(self, &insn);

                // Check if it is necessary to increment the PC.
                // If not, this has already been done by the instruction itself.
                if self.increment_pc {
                    self.registers[PC] += insn.len() as u32;
                }
            }
            None => return,
        }
    }
}

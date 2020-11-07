//! Falcon microprocessor abstractions.

mod instructions;
mod pipeline;
mod registers;

use std::thread::sleep;
use std::time::Duration;

use crate::cpu::instructions::process_instruction;
pub use crate::cpu::registers::*;
use crate::crypto::Scp;
use crate::dma;
use crate::memory::{LookupError, Memory, PageFlag};

/// Representation of the Falcon processor.
pub struct Cpu {
    /// The Falcon revision to emulate.
    version: usize,
    /// The Falcon CPU registers.
    pub registers: CpuRegisters,
    /// The Falcon SRAM for code and data.
    pub memory: Memory,
    /// The clock period it takes the Falcon to walk through a single CPU cycle.
    clock_period: Duration,
    /// The Falcon DMA engine.
    dma_engine: dma::Engine,
    /// The current execution state of the processor that controls the way
    /// the CPU executes code.
    state: ExecutionState,
    /// The Secure Co-Processor responsible for cryptographic operations.
    scp: Scp,
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

/// The execution state of the Falcon processor which controls its behavior.
///
/// The execution states influence code execution and how interrupts are
/// being handled. There are different ways to change the processor state,
/// including resets, instructions, interrupts, and host interaction.
pub enum ExecutionState {
    /// The processor is actively running and executes instructions.
    Running,
    /// The processor is stopped and executes no instructions.
    ///
    /// In this state, the processor is finally halted until it gets
    /// reset and restarted by the host system.
    Stopped,
    /// The processor is sleeping and executes no instructions.
    ///
    /// In this state, the processor is halted, but internal interrupts
    /// can restart execution.
    Sleeping,
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
    pub fn new(version: usize, imem_size: u32, dmem_size: u32, clock_period: Duration) -> Self {
        Cpu {
            version,
            registers: CpuRegisters::new(),
            memory: Memory::new(imem_size, dmem_size),
            clock_period,
            dma_engine: dma::Engine::new(),
            state: ExecutionState::Stopped,
            scp: Scp::new(),
            increment_pc: false,
        }
    }

    /// Returns the Falcon CPU revision that is being emulated.
    pub fn get_version(&self) -> usize {
        self.version
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
        self.registers[CSW] |= 1 << 24;

        // Store the trap status, composed of the current PC and the trap reason.
        self.registers[EXCI] = self.registers[PC] | ((trap as u8 & 0xF) as u32) << 20;

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
        self.registers[PC] = self.registers[EV];
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
        self.memory.write_code(address, value);

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

    /// Executes the next instruction at the address held by the PC register.
    pub fn step(&mut self) {
        let insn = match pipeline::fetch_and_decode(self, self.registers[PC]) {
            Ok(insn) => insn,
            Err(e) => match e {
                pipeline::PipelineError::FetchingFailed(lookup_error) => {
                    self.trigger_trap(if lookup_error == LookupError::NoPageHits {
                        Trap::VmNoHit
                    } else {
                        Trap::VmMultiHit
                    });

                    return;
                }
                pipeline::PipelineError::DecodingFailed(decoding_error) => match decoding_error {
                    faucon_asm::Error::UnknownInstruction(_) => {
                        self.trigger_trap(Trap::InvalidOpcode);

                        return;
                    }
                    faucon_asm::Error::IoError => panic!("You should hopefully never see this."),
                    faucon_asm::Error::Eof => return,
                },
            },
        };

        // Process the instruction and sleep for the consumed amount of cycles.
        let cycles = process_instruction(self, &insn);
        sleep(self.clock_period * cycles as u32);

        // Increment the Program Counter to the next instruction, if requested.
        // If not requested, it is safe to assume that the instruction modified
        // the PC register to a desired value itself.
        if self.increment_pc {
            self.registers[PC] += insn.len() as u32;
        }
    }
}

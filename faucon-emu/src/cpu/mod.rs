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
}

impl Cpu {
    /// Creates a new instance of the CPU.
    pub fn new() -> Self {
        Cpu {
            registers: CpuRegisters::new(),
            memory: Memory::new(),
            dma_engine: dma::Engine::new(),
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

    fn fetch_insn(&self, address: u32) -> faucon_asm::Result<Instruction> {
        // Look up the TLB to get the physical code page.
        let (page_index, tlb) = match self.memory.tlb.lookup(address) {
            Ok((page, tlb)) => (page, tlb),
            Err(LookupError::NoPageHits) => todo!("Generate trap"),
            Err(LookupError::MultiplePageHits) => todo!("Generate trap"),
        };

        // Build the physical code address to read from.
        let page_offset = (address & 0xFF) as u16;
        let code_address = ((page_index as u16) << 8) | page_offset;

        // If the page is marked usable, complete the access using the physical page.
        if tlb.get_flag(PageFlag::Usable) {
            let mut code = &self.memory.code[code_address as usize..];
            return Ok(disassembler::read_instruction(&mut code)?);
        } else if tlb.get_flag(PageFlag::Busy) {
            // The page is marked busy, the access must be completed when possible.
            todo!("Wait for the page to be marked as usable");
        }

        // If the page wouldn't have any flags at all, the method
        // would already opt out at TLB lookup.
        unreachable!();
    }

    /// Executes the next instruction at the address held by the PC register.
    pub fn step(&mut self) {
        match self.fetch_insn(self.registers[PC]) {
            Ok(insn) => {
                process_instruction(self, &insn);

                self.registers[PC] += insn.len() as u32;
            }
            Err(faucon_asm::Error::UnknownInstruction(_)) => todo!("Generate trap"),
            _ => todo!("Handle these errors in a sane way"),
        }
    }
}

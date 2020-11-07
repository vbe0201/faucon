//! Implementation of the multi-stage pipeline for executing code.

use faucon_asm::read_instruction;

use super::{Cpu, ExecutionState};
use crate::memory::{LookupError, PageFlag};

/// Possible errors related to the Falcon execution pipeline
#[derive(Debug, PartialEq)]
pub enum PipelineError {
    /// An error related to virtual memory management on instruction lookup.
    FetchingFailed(LookupError),
    /// An error related to decoding instruction bytes in memory.
    DecodingFailed(faucon_asm::Error),
    /// The processor is in a halted state and cannot execute any instructions.
    CpuHalted,
}

/// Fetches the next instruction from the given virtual address, if possible.
///
/// The Falcon pipeline simplified consists of six stages:
/// Fetch -> Decode -> Calculate Operands -> Fetch Operands -> Execute -> Write Operands
///
/// However, for emulation in faucon, it essentially lowers down to three main stages:
/// Fetch -> Decode -> Execute
///
/// This function carries out the first two of those stages and returns either
/// the decoded instruction or an error that gives details about what went wrong.
///
/// Actual execution of an instruction devolves around handling the other stages fluently.
pub fn fetch_and_decode(
    cpu: &mut Cpu,
    address: u32,
) -> Result<faucon_asm::Instruction, PipelineError> {
    if cpu.state == ExecutionState::Stopped {
        return Err(PipelineError::CpuHalted);
    }

    // Translate the virtual address into a physical address to read from.
    let (physical_address, tlb) = match cpu.memory.tlb.lookup(address) {
        Ok((page, tlb)) => {
            // Build the physical address that corresponds matching the virtual address.
            let page_offset = (address & 0xFF) as usize;
            let code_address = ((page as usize) << 8) | page_offset;

            (code_address, tlb)
        }
        Err(e) => return Err(PipelineError::FetchingFailed(e)),
    };

    if tlb.get_flag(PageFlag::Usable) {
        // Read the next instruction from the given address in IMEM.
        let mut code = &cpu.memory.code[physical_address..];
        match read_instruction(&mut code) {
            Ok(insn) => Ok(insn),
            Err(e) => Err(PipelineError::DecodingFailed(e)),
        }
    } else {
        // Since faucon does not use any form of parallelism in its tasks, unlike a real
        // Falcon, a state where a page would be queried while being marked as incomplete
        // is impossible at this point.
        unreachable!()
    }
}

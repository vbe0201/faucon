use faucon_asm::{Instruction, InstructionKind};

use super::Cpu;

mod data;
mod utils;
mod vm;

/// Processes the given instruction on the microprocessor and returns the amount
/// of CPU cycles the operation took.
pub fn process_instruction(cpu: &mut Cpu, insn: &Instruction) -> usize {
    match insn.kind() {
        InstructionKind::LD => data::ld(cpu, insn),
        InstructionKind::ST => data::st(cpu, insn),
        InstructionKind::PUSH => data::push(cpu, insn),
        InstructionKind::POP => data::pop(cpu, insn),
        InstructionKind::PTLB => vm::ptlb(cpu, insn),
        InstructionKind::VTLB => vm::vtlb(cpu, insn),
        InstructionKind::ITLB => vm::itlb(cpu, insn),
        _ => unimplemented!(),
    }
}

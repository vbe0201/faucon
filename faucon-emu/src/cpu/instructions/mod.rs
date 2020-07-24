use faucon_asm::{Instruction, InstructionKind};

use super::Cpu;

mod data;
mod utils;
mod vm;

/// Processes the given instruction on the microprocessor and returns the amount
/// of CPU cycles the operation took.
pub fn process_instruction(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let handler = get_handler(insn);
    handler(cpu, insn)
}

fn get_handler(insn: &Instruction) -> impl Fn(&mut Cpu, &Instruction) -> usize {
    match insn.kind() {
        InstructionKind::LD => data::ld,
        InstructionKind::ST => data::st,
        InstructionKind::PUSH => data::push,
        InstructionKind::POP => data::pop,
        InstructionKind::PTLB => vm::ptlb,
        InstructionKind::VTLB => vm::vtlb,
        InstructionKind::ITLB => vm::itlb,
        _ => unimplemented!(),
    }
}

use faucon_asm::{Instruction, InstructionKind};

use super::*;

mod alu;
mod branch;
mod control;
mod data;
mod intr;
mod utils;
mod vm;

/// Processes the given instruction on the microprocessor and returns the amount
/// of CPU cycles the operation took.
pub fn process_instruction(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let handler = get_handler(insn);
    handler(cpu, insn)
}

fn get_handler(insn: &Instruction) -> impl FnOnce(&mut Cpu, &Instruction) -> usize {
    match insn.kind() {
        InstructionKind::CLEAR => alu::clear,
        InstructionKind::MOV => control::mov,
        InstructionKind::CALL => branch::call,
        InstructionKind::LCALL => branch::call,
        InstructionKind::LJMP => branch::jmp,
        InstructionKind::RET => branch::ret,
        InstructionKind::LD => data::ld,
        InstructionKind::ST => data::st,
        InstructionKind::PUSH => data::push,
        InstructionKind::POP => data::pop,
        InstructionKind::EXIT => control::exit,
        InstructionKind::SLEEP => control::sleep,
        InstructionKind::PTLB => vm::ptlb,
        InstructionKind::VTLB => vm::vtlb,
        InstructionKind::ITLB => vm::itlb,
        InstructionKind::IRET => intr::iret,
        InstructionKind::TRAP => intr::trap,
        _ => unimplemented!(),
    }
}

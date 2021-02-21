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
        InstructionKind::CMPU => alu::cmp,
        InstructionKind::CMPS => alu::cmp,
        InstructionKind::CMP => alu::cmp,
        InstructionKind::ADD => alu::addsub,
        InstructionKind::ADC => alu::addsub,
        InstructionKind::SUB => alu::addsub,
        InstructionKind::SBB => alu::addsub,
        InstructionKind::SHL => alu::shift,
        InstructionKind::SHLC => alu::shift,
        InstructionKind::SAR => alu::shift,
        InstructionKind::SHR => alu::shift,
        InstructionKind::SHRC => alu::shift,
        InstructionKind::NOT => alu::unary,
        InstructionKind::NEG => alu::unary,
        InstructionKind::HSWAP => alu::unary,
        InstructionKind::SETHI => alu::sethi,
        InstructionKind::CLEAR => alu::clear,
        InstructionKind::MULU => alu::mul,
        InstructionKind::MULS => alu::mul,
        InstructionKind::SEXT => alu::sext,
        InstructionKind::AND => alu::bitwise,
        InstructionKind::OR => alu::bitwise,
        InstructionKind::XOR => alu::bitwise,
        InstructionKind::XBIT => alu::xbit,
        InstructionKind::BSET => alu::bitop,
        InstructionKind::BCLR => alu::bitop,
        InstructionKind::BTGL => alu::bitop,
        InstructionKind::DIV => alu::divmod,
        InstructionKind::MOD => alu::divmod,
        InstructionKind::SETP => alu::setp,
        InstructionKind::MOV => control::mov,
        InstructionKind::CALL => branch::call,
        InstructionKind::LCALL => branch::call,
        InstructionKind::LBRA => branch::bra,
        InstructionKind::RET => branch::ret,
        InstructionKind::LD => data::ld,
        InstructionKind::ST => data::st,
        InstructionKind::PUSH => data::push,
        InstructionKind::POP => data::pop,
        InstructionKind::MPUSH => data::mpush,
        InstructionKind::MPOP => data::mpop,
        InstructionKind::MPOPADD => data::mpopadd,
        InstructionKind::MPOPRET => data::mpopret,
        InstructionKind::MPOPADDRET => data::mpopaddret,
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

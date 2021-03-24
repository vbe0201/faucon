//! Instructions related to Falcon code branching.

use faucon_asm::Instruction;

use super::{utils, Cpu, PC};

/// Performs a (long) subroutine call to an absolute target address.
pub fn call(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Extract the instruction operands (single register or immediate).
    let target = insn.operands()[0];

    // Push return address onto the stack.
    cpu.stack_push(cpu.registers[PC] + insn.raw_bytes().unwrap().len() as u32);

    // Branch to the absolute address.
    cpu.registers[PC] = utils::get_value(cpu, insn.operand_size(), target);

    // Signal irregular PC increment to the CPU.
    cpu.increment_pc = false;

    4
}

/// Performs a (long) unconditional branch to an absolute target address.
pub fn bra(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Extract the instruction operands (single register or immediate).
    let target = insn.operands()[0];

    // Branch to the absolute address.
    cpu.registers[PC] = utils::get_value(cpu, insn.operand_size(), target);

    // Signal irregular PC increment to the CPU.
    cpu.increment_pc = false;

    4
}

/// Returns from a previous (long) call.
pub fn ret(cpu: &mut Cpu, _: &Instruction) -> usize {
    // Restore the return address from the stack.
    cpu.registers[PC] = cpu.stack_pop();

    // Signal irregular PC increment to the CPU.
    cpu.increment_pc = false;

    5
}

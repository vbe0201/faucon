//! Instructions related to Falcon code branching.

use faucon_asm::{Instruction, Operand};

use super::{Cpu, PC};

/// Performs a (long) subroutine call to an absolute target address.
pub fn call(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Extract the instruction operands (single register or immediate).
    let target = insn.operands()[0];

    // Push return address onto the stack.
    cpu.stack_push(cpu.registers[PC] + insn.len() as u32);

    // Branch to the absolute address.
    cpu.registers[PC] = match target {
        Operand::Register(reg) => cpu.registers[reg],
        Operand::I8(imm) => imm as u32,
        Operand::I16(imm) => imm as u32,
        Operand::I24(imm) | Operand::I32(imm) => imm,
        _ => panic!("Invalid instruction hit"),
    };

    // Signal irregular PC increment to the CPU.
    cpu.increment_pc = false;

    4
}

/// Performs a (long) unconditional branch to an absolute target address.
pub fn jmp(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Extract the instruction operands (single register or immediate).
    let target = insn.operands()[0];

    // Branch to the absolute address.
    cpu.registers[PC] = match target {
        Operand::Register(reg) => cpu.registers[reg],
        Operand::I8(imm) => imm as u32,
        Operand::I16(imm) => imm as u32,
        Operand::I24(imm) | Operand::I32(imm) => imm,
        _ => panic!("Invalid instruction hit"),
    };

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

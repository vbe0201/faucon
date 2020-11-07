//! Instructions related to interfacing with the Falcon data segment.

use faucon_asm::{Instruction, Operand, Register, RegisterKind};

use super::{branch::ret, utils, Cpu, SP};

/// Loads a value from data segment to a register.
pub fn ld(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register and memory access descriptor).
    let destination = operands[0];
    let source = operands[1];

    // Read the value from DMem and store it in the destination register.
    utils::write_reg(cpu, insn.operand_size(), destination, source);

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

/// Stores a value from a register to data segment.
pub fn st(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (memory access descriptor and register).
    let destination = operands[0];
    let source = operands[1];

    // Write the value in the source register to DMem.
    utils::write_mem(cpu, insn.operand_size(), source, destination);

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

/// Pushes a given register onto the stack.
pub fn push(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Extract the instruction operand (a single register).
    let source = insn.operands()[0];

    // Push the word in the supplied register onto the stack.
    cpu.stack_push(cpu.registers[source]);

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

/// Pops a value off the stack and stores the result in a register.
pub fn pop(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Extract the instruction operand (a single register).
    let destination = insn.operands()[0];

    // Pop a value off the stack and store it in the destination register.
    cpu.registers[destination] = cpu.stack_pop();

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

/// Pushes a specified range of registers onto the stack.
pub fn mpush(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Extract the instruction operand (a single register).
    if let Operand::Register(reg) = insn.operands()[0] {
        // Push the described range of registers onto the stack.
        for i in 0..reg.1 {
            let fake_reg = Register(RegisterKind::Gpr, i);
            cpu.stack_push(cpu.registers[fake_reg]);
        }
    } else {
        unreachable!();
    }

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1 // TODO: Is this really one cycle?
}

/// Pops a specified range of registers off the stack.
pub fn mpop(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Extract the instruction operand (a single register).
    if let Operand::Register(reg) = insn.operands()[0] {
        // Pop the described range of registers off the stack.
        for i in 0..reg.1 {
            let fake_reg = Register(RegisterKind::Gpr, i);
            cpu.registers[fake_reg] = cpu.stack_pop();
        }
    } else {
        unreachable!();
    }

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1 // TODO: Is this really one cycle?
}

/// Pops a specified range of registers off the stack and adds an immediate to $sp.
pub fn mpopadd(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Execute the mpop instruction using the first operand (a register).
    mpop(cpu, insn);

    // Add the second operand (an immediate) to the $sp register.
    if let Operand::I32(imm) = insn.operands()[1] {
        cpu.registers[SP] = cpu.registers[SP].wrapping_add(imm);
    } else {
        unreachable!();
    }

    2
}

/// Pops a specified range of registers off the stack and returns.
pub fn mpopret(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Execute the mpop instruction using the first operand (a register).
    mpop(cpu, insn);

    // Execute the ret instruction.
    ret(cpu, insn);

    2
}

/// Pops a specified range of registers off the stack, adds an immediate to $sp and returns.
pub fn mpopaddret(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Execute the mpopadd instruction using the instruction operands.
    mpopadd(cpu, insn);

    // Execute the ret instruction.
    ret(cpu, insn);

    2
}

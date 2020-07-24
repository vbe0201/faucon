//! Instructions related to interfacing with the Falcon data segment.

use faucon_asm::Instruction;

use super::utils;
use super::Cpu;

/// Loads a value from data segment to a register.
pub fn ld(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register and memory access descriptor).
    let destination = operands[0];
    let source = operands[1];

    // Read the value from DMem and store it in the destination register.
    utils::memory_to_register(cpu, insn.operand_size, source, destination);

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
    utils::register_to_memory(cpu, insn.operand_size, source, destination);

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

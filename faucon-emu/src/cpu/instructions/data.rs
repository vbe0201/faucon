//! Instructions related to interfacing with the Falcon data segment.

use faucon_asm::Instruction;

use super::Cpu;

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

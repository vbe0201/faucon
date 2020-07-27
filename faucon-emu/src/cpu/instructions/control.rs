//! Instructions related to controlling the state of the processor.

use faucon_asm::Instruction;

use super::{utils, Cpu};

/// Copies a value into another register.
pub fn mov(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register and register or immediate).
    let destination = operands[0];
    let source = operands[1];

    // Copy the source value to the destination.
    utils::write_reg(cpu, insn.operand_size, destination, source);

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

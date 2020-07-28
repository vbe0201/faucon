//! Arithmetic Falcon instructions.

use faucon_asm::{Instruction, Operand};

use super::{utils, Cpu};

pub fn clear(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Extract the instruction operands (a single register).
    let destination = insn.operands()[0];

    // Clear the register.
    utils::write_reg(cpu, insn.operand_size, destination, Operand::I8(0));

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

//! Arithmetic Falcon instructions.

use enum_primitive::FromPrimitive;
use faucon_asm::{Instruction, Operand};

use super::{utils, Cpu, CpuFlag};

pub fn clear(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Extract the instruction operands (a single register).
    let destination = insn.operands()[0];

    // Clear the register.
    utils::write_reg(cpu, insn.operand_size, destination, Operand::I8(0));

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

/// Modifies a bit in a register.
pub fn xbit(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register, register, immediate/register/flag).
    let destination = operands[0];
    let source1 = operands[1];
    let source2 = operands[2];

    // Set the bit accordingly.
    let bit = match source2 {
        Operand::Register(reg) => cpu.registers[reg] & 0x1FF,
        Operand::Flag(flag) => flag as u32,
        Operand::I8(imm) => imm as u32,
        _ => unreachable!(),
    };
    cpu.registers[destination] = cpu.registers[source1] >> bit & 1;

    // Set the ALU flags accordingly.
    cpu.registers.set_flag(CpuFlag::OVERFLOW, false);
    cpu.registers
        .set_flag(CpuFlag::ZERO, cpu.registers[destination] == 0);

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

/// Sets a specific CPU flag to a given value.
pub fn setp(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register or flag and register).
    let source1 = operands[0];
    let source2 = operands[1];

    // Get the bit in question and determine the value to set it to.
    let value = cpu.registers[source2] & 1 != 0;
    let flag = if insn.opcode() == 0xF2 {
        utils::parse_flag(source1).unwrap()
    } else {
        CpuFlag::from_u32(cpu.registers[source1] & 0x1F).unwrap()
    };

    // Set the bit accordingly.
    cpu.registers.set_flag(flag, value);

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

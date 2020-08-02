//! Arithmetic Falcon instructions.

use enum_primitive::FromPrimitive;
use faucon_asm::{Instruction, InstructionKind, Operand, OperandSize};

use super::{utils, Cpu, CpuFlag};

fn sign(x: u32, size: OperandSize) -> bool {
    (x >> (size.value() as u32 - 1) & 1) != 0
}

/// Sets the high 16 bits of a register ot a given value.
pub fn sethi(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register and immediate).
    let destination = operands[0];
    let source = utils::get_value(cpu, insn.operand_size, operands[1]);

    // Store the source value in the high 16 bits of the destination register.
    cpu.registers[destination] = cpu.registers[destination] & 0xFFFF | source << 0x10;

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

/// Clears a given CPU register.
pub fn clear(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Extract the instruction operands (a single register).
    let destination = insn.operands()[0];

    // Clear the register.
    cpu.registers[destination] = 0;

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

/// Multiplies two operands and stores the result.
pub fn mul(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register, register and register or immediate).
    let destination = operands[0];
    let mut source1 = utils::get_value(cpu, insn.operand_size, operands[1]) & 0xFFFF;
    let mut source2 = utils::get_value(cpu, insn.operand_size, operands[2]) & 0xFFFF;

    // If the instruction is MULS, sign-extend the operands properly.
    if insn.kind() == InstructionKind::MULS {
        if source1 & 0x8000 != 0 {
            source1 |= 0xFFFF0000;
        }
        if source2 & 0x8000 != 0 {
            source2 |= 0xFFFF0000;
        }
    }

    // Perform the multiplication and store the result.
    cpu.registers[destination] = source1.wrapping_mul(source2);

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

/// Performs a sign-extension of the given operand.
pub fn sext(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register, register and register or immediate).
    let destination = operands[0];
    let source1 = utils::get_value(cpu, insn.operand_size, operands[1]);
    let source2 = utils::get_value(cpu, insn.operand_size, operands[2]);

    // Perform the sign-extension and store the result.
    let bit = source2 & 0x1F;
    if source1 & 1 << bit != 0 {
        cpu.registers[destination] = source1 & ((1 << bit) - 1) | (-(1 << bit as i32)) as u32;
    } else {
        cpu.registers[destination] = source1 & ((1 << bit) - 1);
    }

    // Store the CPU flags accordingly.
    cpu.registers.set_flag(
        CpuFlag::NEGATIVE,
        sign(cpu.registers[destination], insn.operand_size),
    );
    cpu.registers
        .set_flag(CpuFlag::ZERO, cpu.registers[destination] == 0);

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

/// Performs a bitwise operation on two operands and stores the result.
pub fn bitwise(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register, register and register or immediate).
    let destination = operands[0];
    let source1 = utils::get_value(cpu, insn.operand_size, operands[1]);
    let source2 = utils::get_value(cpu, insn.operand_size, operands[2]);

    // Perform the calculation and store the result.
    match insn.kind() {
        InstructionKind::AND => cpu.registers[destination] = source1 & source2,
        InstructionKind::OR => cpu.registers[destination] = source1 | source2,
        InstructionKind::XOR => cpu.registers[destination] = source1 ^ source2,
        _ => unreachable!(),
    };

    // Set CPU flags accordingly.
    cpu.registers.set_flag(CpuFlag::CARRY, false);
    cpu.registers.set_flag(CpuFlag::OVERFLOW, false);
    cpu.registers.set_flag(
        CpuFlag::NEGATIVE,
        sign(cpu.registers[destination], insn.operand_size),
    );
    cpu.registers
        .set_flag(CpuFlag::ZERO, cpu.registers[destination] == 0);

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
        Operand::I8(imm) => imm as u32 & 0x1FF,
        _ => unreachable!(),
    };
    cpu.registers[destination] = cpu.registers[source1] >> bit & 1;

    // Set the ALU flags accordingly.
    cpu.registers.set_flag(CpuFlag::NEGATIVE, false);
    cpu.registers
        .set_flag(CpuFlag::ZERO, cpu.registers[destination] == 0);

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

/// Modifies a given bit in a register.
pub fn bitop(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register and register or immediate).
    let destination = operands[0];
    let source = operands[1];

    // Extract the bit and perform the operation.
    let bit = match source {
        Operand::Register(reg) => cpu.registers[reg] & 0x1FF,
        Operand::Flag(flag) => flag as u32,
        Operand::I8(imm) => imm as u32 & 0x1FF,
        _ => unreachable!(),
    };

    match insn.kind() {
        InstructionKind::BSET => cpu.registers[destination] |= 1 << bit,
        InstructionKind::BCLR => cpu.registers[destination] &= !(1 << bit),
        InstructionKind::BTGL => cpu.registers[destination] ^= 1 << bit,
        _ => unreachable!(),
    };

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

/// Performs a division or takes the modulus of two operands.
pub fn divmod(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register, register and register or immediate).
    let destination = operands[0];
    let source1 = utils::get_value(cpu, insn.operand_size, operands[1]);
    let source2 = utils::get_value(cpu, insn.operand_size, operands[2]);

    // Divide both operands and handle unsupported divisions by zero.
    let div_result = if source2 == 0 {
        0xFFFFFFFF
    } else {
        source1 / source2
    };

    // Finalize the operation and store the result accordingly to the instruction.
    match insn.kind() {
        InstructionKind::DIV => cpu.registers[destination] = div_result,
        InstructionKind::MOD => cpu.registers[destination] = source1 - div_result * source2,
        _ => unreachable!(),
    };

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    30
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

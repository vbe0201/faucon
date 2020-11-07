//! Arithmetic Falcon instructions.

use enum_primitive::FromPrimitive;
use faucon_asm::{Instruction, InstructionKind, Operand, OperandSize};

use super::{utils, Cpu, CpuFlag};

fn sign(x: u32, size: OperandSize) -> bool {
    (x >> (size.value() as u32 - 1) & 1) != 0
}

fn carry(a: bool, b: bool, c: bool) -> bool {
    // If a and b are both set, there is always carry out.
    if a && b {
        return true;
    }

    // One of a and b is set. In this case, there is carry out if
    // the result has bit 0 set.
    if a || b && !c {
        return true;
    }

    // Neither a nor b is set, there is no possibility of carry out.
    false
}

fn overflow(a: bool, b: bool, c: bool) -> bool {
    a == b && a != c
}

/// Compares two operands and stores ALU flags based on the result.
pub fn cmp(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register and register or immediate).
    let source1 = utils::get_value(cpu, insn.operand_size(), operands[0]);
    let source2 = utils::get_value(cpu, insn.operand_size(), operands[1]);

    // Subtract the operands and set ALU flags based on the result.
    let diff = source1.wrapping_sub(source2);
    cpu.registers.set_flag(CpuFlag::ZERO, diff == 0);
    match insn.kind() {
        InstructionKind::CMPS => {
            cpu.registers.set_flag(
                CpuFlag::CARRY,
                overflow(
                    sign(source1, insn.operand_size()),
                    !sign(source2, insn.operand_size()),
                    sign(diff, insn.operand_size()),
                ) ^ sign(diff, insn.operand_size()),
            );
        }
        InstructionKind::CMPU => {
            cpu.registers.set_flag(
                CpuFlag::CARRY,
                !carry(
                    sign(source1, insn.operand_size()),
                    !sign(source2, insn.operand_size()),
                    sign(diff, insn.operand_size()),
                ),
            );
        }
        InstructionKind::CMP => {
            cpu.registers.set_flag(
                CpuFlag::CARRY,
                !carry(
                    sign(source1, insn.operand_size()),
                    !sign(source2, insn.operand_size()),
                    sign(diff, insn.operand_size()),
                ),
            );
            cpu.registers.set_flag(
                CpuFlag::OVERFLOW,
                overflow(
                    sign(source1, insn.operand_size()),
                    !sign(source2, insn.operand_size()),
                    sign(diff, insn.operand_size()),
                ),
            );
            cpu.registers
                .set_flag(CpuFlag::NEGATIVE, sign(diff, insn.operand_size()));
        }
        _ => unreachable!(),
    };

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

/// Performs an additional or subtraction, based on the instruction, and stores the result.
pub fn addsub(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register, register and register or immediate).
    let destination = operands[0];
    let source1 = utils::get_value(cpu, insn.operand_size(), operands[1]);
    let source2 = utils::get_value(cpu, insn.operand_size(), operands[2]);

    // Perform the operation.
    let c = cpu.registers.get_flag(CpuFlag::CARRY) as u32;
    let res = match insn.kind() {
        InstructionKind::ADD => source1.wrapping_add(source2),
        InstructionKind::ADC => source1.wrapping_add(source2).wrapping_add(c),
        InstructionKind::SUB => source1.wrapping_sub(source2),
        InstructionKind::SBB => source1.wrapping_sub(source2).wrapping_sub(c),
        _ => unreachable!(),
    };

    // Store some ALU flags based on the operands and the result.
    match insn.kind() {
        InstructionKind::ADD | InstructionKind::ADC => {
            cpu.registers.set_flag(
                CpuFlag::CARRY,
                carry(
                    sign(source1, insn.operand_size()),
                    sign(source2, insn.operand_size()),
                    sign(res, insn.operand_size()),
                ),
            );
            cpu.registers.set_flag(
                CpuFlag::OVERFLOW,
                overflow(
                    sign(source1, insn.operand_size()),
                    sign(source2, insn.operand_size()),
                    sign(res, insn.operand_size()),
                ),
            );
        }
        InstructionKind::SUB | InstructionKind::SBB => {
            cpu.registers.set_flag(
                CpuFlag::CARRY,
                !carry(
                    sign(source1, insn.operand_size()),
                    !sign(source2, insn.operand_size()),
                    sign(res, insn.operand_size()),
                ),
            );
            cpu.registers.set_flag(
                CpuFlag::OVERFLOW,
                overflow(
                    sign(source1, insn.operand_size()),
                    !sign(source2, insn.operand_size()),
                    sign(res, insn.operand_size()),
                ),
            );
        }
        _ => unreachable!(),
    };

    // Store the result value accordingly.
    utils::write_value_to_reg(cpu, insn.operand_size(), destination, res);

    // Set the remaining ALU flags.
    cpu.registers.set_flag(
        CpuFlag::NEGATIVE,
        sign(cpu.registers[destination], insn.operand_size()),
    );
    cpu.registers
        .set_flag(CpuFlag::ZERO, cpu.registers[destination] == 0);

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

/// Carries out a bitwise shift and stores the result.
pub fn shift(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register, register and register or immediate).
    let destination = operands[0];
    let source1 = utils::get_value(cpu, insn.operand_size(), operands[1]);
    let mut source2 = utils::get_value(cpu, insn.operand_size(), operands[2]);

    // Truncate source2 accordingly, depending on the operand size.
    match insn.operand_size() {
        OperandSize::EightBit => source2 &= 0x7,
        OperandSize::SixteenBit => source2 &= 0xF,
        OperandSize::ThirtyTwoBit => source2 &= 0x1F,
        _ => unreachable!(),
    };

    // Carry out the operation and store the result.
    let res = match insn.kind() {
        InstructionKind::SHL | InstructionKind::SHLC => {
            let mut result = source1.wrapping_shl(source2);

            if insn.kind() == InstructionKind::SHLC && source2 != 0 {
                result |= (cpu.registers.get_flag(CpuFlag::CARRY) as u32) << (source2 - 1);
            }

            if source2 == 0 {
                cpu.registers.set_flag(CpuFlag::CARRY, false);
            } else {
                cpu.registers.set_flag(
                    CpuFlag::CARRY,
                    (source1 >> (insn.operand_size().value() as u32 - source2) & 1) != 0,
                );
            }

            result
        }
        InstructionKind::SHR | InstructionKind::SAR | InstructionKind::SHRC => {
            let mut result = source1.wrapping_shr(source2);

            if insn.kind() == InstructionKind::SHRC && source2 != 0 {
                result |= (cpu.registers.get_flag(CpuFlag::CARRY) as u32)
                    << (insn.operand_size().value() as u32 - source2);
            } else if insn.kind() == InstructionKind::SAR && sign(source1, insn.operand_size()) {
                result |= !0 << (insn.operand_size().value() as u32 - source2);
            }

            if source2 == 0 {
                cpu.registers.set_flag(CpuFlag::CARRY, false);
            } else {
                cpu.registers
                    .set_flag(CpuFlag::CARRY, ((source2 - 1) & 1) != 0);
            }

            result
        }
        _ => unreachable!(),
    };

    // Store the result value accordingly.
    utils::write_value_to_reg(cpu, insn.operand_size(), destination, res);

    // Set the remaining ALU flags.
    cpu.registers.set_flag(CpuFlag::OVERFLOW, false);
    cpu.registers.set_flag(
        CpuFlag::NEGATIVE,
        sign(cpu.registers[destination], insn.operand_size()),
    );
    cpu.registers
        .set_flag(CpuFlag::ZERO, cpu.registers[destination] == 0);

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

/// Performs a unary binary operation.
pub fn unary(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register and immediate).
    let destination = operands[0];
    let source = utils::get_value(cpu, insn.operand_size(), operands[1]);

    // Carry out the operation and store the result.
    match insn.kind() {
        InstructionKind::NOT => {
            cpu.registers[destination] = !source;
            cpu.registers.set_flag(CpuFlag::OVERFLOW, false);
        }
        InstructionKind::NEG => {
            cpu.registers[destination] = source.wrapping_neg();
            cpu.registers.set_flag(
                CpuFlag::OVERFLOW,
                cpu.registers[destination] == (1 << (insn.operand_size().value() - 1)) as u32,
            );
        }
        InstructionKind::HSWAP => {
            cpu.registers[destination] = source >> (insn.operand_size().value() / 2) as u32
                | source << (insn.operand_size().value() / 2) as u32;
            cpu.registers
                .set_flag(CpuFlag::OVERFLOW, cpu.registers[destination] == 0);
        }

        _ => unreachable!(),
    };

    // Set the remaining ALU flags.
    cpu.registers.set_flag(
        CpuFlag::NEGATIVE,
        sign(cpu.registers[destination], insn.operand_size()),
    );
    cpu.registers
        .set_flag(CpuFlag::ZERO, cpu.registers[destination] == 0);

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}

/// Sets the high 16 bits of a register ot a given value.
pub fn sethi(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register and immediate).
    let destination = operands[0];
    let source = utils::get_value(cpu, insn.operand_size(), operands[1]);

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
    let mut source1 = utils::get_value(cpu, insn.operand_size(), operands[1]) & 0xFFFF;
    let mut source2 = utils::get_value(cpu, insn.operand_size(), operands[2]) & 0xFFFF;

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
    let source1 = utils::get_value(cpu, insn.operand_size(), operands[1]);
    let source2 = utils::get_value(cpu, insn.operand_size(), operands[2]);

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
        sign(cpu.registers[destination], insn.operand_size()),
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
    let source1 = utils::get_value(cpu, insn.operand_size(), operands[1]);
    let source2 = utils::get_value(cpu, insn.operand_size(), operands[2]);

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
        sign(cpu.registers[destination], insn.operand_size()),
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
    let source1 = utils::get_value(cpu, insn.operand_size(), operands[1]);
    let source2 = utils::get_value(cpu, insn.operand_size(), operands[2]);

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

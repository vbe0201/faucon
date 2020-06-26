use faucon_asm::instruction::InstructionKind;
use faucon_asm::{Instruction, Operand};

use crate::cpu::{Cpu, CpuFlag};

/// Macro to expand an operand to a given expression when it is guaranteed that
/// it takes a specific variant.
macro_rules! operand {
    ($operand:expr, $($variant:pat)|* => $result:expr) => {
        if false {
            unreachable!()
        } $(else if let $variant = $operand {
            Some($result)
        })* else {
            None
        }
    };
}

/// Checks if the sign bit of an instruction result is set.
///
/// This is necessary to determine whether the sign flag
/// should be set.
fn is_signed(x: u32, insn: &Instruction) -> bool {
    let sz: u32 = insn.operand_size().into();

    (x >> (sz - 1) & 1) != 0
}

/// Emulates a given CPU instruction.
///
/// Returns the amount of CPU cycles that the instruction took.
pub fn process_instruction(cpu: &mut Cpu, insn: &Instruction) -> usize {
    match insn.kind {
        InstructionKind::AND(_, _, _) => and(cpu, insn),
        InstructionKind::OR(_, _, _) => or(cpu, insn),
        InstructionKind::XOR(_, _, _) => xor(cpu, insn),
        InstructionKind::XBIT(_, _, _) => xbit(cpu, insn),
        InstructionKind::BSET(_, _, _) => bset(cpu, insn),
        InstructionKind::BCLR(_, _, _) => bclr(cpu, insn),
        InstructionKind::BTGL(_, _, _) => btgl(cpu, insn),
        _ => todo!("Emulate remaining instructions"),
    }
}

/// Executes an AND instruction.
fn and(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands().unwrap();

    // Extract the operands required to perform the operation.
    let destination = operand!(operands[0], Operand::Register(reg) => reg).unwrap();
    let source1 = match insn.opcode() {
        0xC0 | 0xE0 | 0xFF => operand!(operands[1], Operand::Register(reg) => reg).unwrap(),
        0xF0 | 0xF1 | 0xFD => destination,
        _ => unreachable!(),
    };
    let source2 = operand!(match insn.opcode() {
        0xC0 | 0xE0 | 0xFF => operands[2],
        0xF0 | 0xF1 | 0xFD => operands[1],
        _ => unreachable!(),
    }, Operand::I8(int) | Operand::I16(int) => int as u32)
    .unwrap();

    // Compute the result of the operation and store it.
    let result = cpu.registers.get_gpr(source1.value) & source2;
    cpu.registers.set_gpr(destination.value, result);

    // Set the CPU flags accordingly.
    cpu.registers.set_flag(CpuFlag::CARRY, false);
    cpu.registers.set_flag(CpuFlag::OVERFLOW, false);
    cpu.registers
        .set_flag(CpuFlag::NEGATIVE, is_signed(result, insn));
    cpu.registers.set_flag(CpuFlag::ZERO, result == 0);

    1
}

/// Executes an OR instruction.
fn or(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands().unwrap();

    // Extract the operands required to perform the operation.
    let destination = operand!(operands[0], Operand::Register(reg) => reg).unwrap();
    let source1 = match insn.opcode() {
        0xC0 | 0xE0 | 0xFF => operand!(operands[1], Operand::Register(reg) => reg).unwrap(),
        0xF0 | 0xF1 | 0xFD => destination,
        _ => unreachable!(),
    };
    let source2 = operand!(match insn.opcode() {
        0xC0 | 0xE0 | 0xFF => operands[2],
        0xF0 | 0xF1 | 0xFD => operands[1],
        _ => unreachable!(),
    }, Operand::I8(int) | Operand::I16(int) => int as u32)
    .unwrap();

    // Compute the result of the operation and store it.
    let result = cpu.registers.get_gpr(source1.value) | source2;
    cpu.registers.set_gpr(destination.value, result);

    // Set the CPU flags accordingly.
    cpu.registers.set_flag(CpuFlag::CARRY, false);
    cpu.registers.set_flag(CpuFlag::OVERFLOW, false);
    cpu.registers
        .set_flag(CpuFlag::NEGATIVE, is_signed(result, insn));
    cpu.registers.set_flag(CpuFlag::ZERO, result == 0);

    1
}

/// Executes an XOR instruction.
fn xor(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands().unwrap();

    // Extract the operands required to perform the operation.
    let destination = operand!(operands[0], Operand::Register(reg) => reg).unwrap();
    let source1 = match insn.opcode() {
        0xC0 | 0xE0 | 0xFF => operand!(operands[1], Operand::Register(reg) => reg).unwrap(),
        0xF0 | 0xF1 | 0xFD => destination,
        _ => unreachable!(),
    };
    let source2 = operand!(match insn.opcode() {
        0xC0 | 0xE0 | 0xFF => operands[2],
        0xF0 | 0xF1 | 0xFD => operands[1],
        _ => unreachable!(),
    }, Operand::I8(int) | Operand::I16(int) => int as u32)
    .unwrap();

    // Compute the result of the operation and store it.
    let result = cpu.registers.get_gpr(source1.value) ^ source2;
    cpu.registers.set_gpr(destination.value, result);

    // Set the CPU flags accordingly.
    cpu.registers.set_flag(CpuFlag::CARRY, false);
    cpu.registers.set_flag(CpuFlag::OVERFLOW, false);
    cpu.registers
        .set_flag(CpuFlag::NEGATIVE, is_signed(result, insn));
    cpu.registers.set_flag(CpuFlag::ZERO, result == 0);

    1
}

/// Executes the XBIT instruction.
fn xbit(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands().unwrap();

    // Extract the operands required to perform the operation.
    let destination = operand!(operands[0], Operand::Register(reg) => reg).unwrap();
    let source1 = match insn.opcode() {
        0xC0 | 0xFF => {
            operand!(operands[1], Operand::Register(reg) => cpu.registers.get_gpr(reg.value))
                .unwrap()
        }
        0xF0 | 0xFE => cpu.registers.get_flags(),
        _ => unreachable!(),
    };
    let source2 = match insn.opcode() {
        0xC0 => operand!(operands[2], Operand::I8(int) => int as u32).unwrap(),
        0xF0 => operand!(operands[1], Operand::I8(int) => int as u32).unwrap(),
        0xFF => operand!(operands[2], Operand::Register(reg) => cpu.registers.get_gpr(reg.value))
            .unwrap(),
        0xFE => operand!(operands[1], Operand::Register(reg) => cpu.registers.get_gpr(reg.value))
            .unwrap(),
        _ => unreachable!(),
    };

    // Compute the result of the operation and store it.
    let result = source1 >> source2 & 1;
    cpu.registers.set_gpr(destination.value, result);

    // Set the CPU flags accordingly.
    cpu.registers.set_flag(CpuFlag::NEGATIVE, false);
    cpu.registers.set_flag(CpuFlag::ZERO, result == 0);

    1
}

/// Executes the BSET instruction.
fn bset(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands().unwrap();

    // Extract the operands required to perform the operation.
    let destination = match insn.opcode() {
        0xF0 | 0xFD => operand!(operands[0], Operand::Register(reg) => reg),
        0xF4 | 0xF9 => None,
        _ => unreachable!(),
    };
    let source = match insn.opcode() {
        0xF0 => operand!(operands[1], Operand::I8(int) => int as u32).unwrap(),
        0xFD => operand!(operands[1], Operand::Register(reg) => cpu.registers.get_gpr(reg.value))
            .unwrap(),
        0xF4 => operand!(operands[0], Operand::I8(int) => int as u32).unwrap(),
        0xF9 => operand!(operands[0], Operand::Register(reg) => cpu.registers.get_gpr(reg.value))
            .unwrap(),
        _ => unreachable!(),
    };

    // Compute the result of the operation and store it.
    let bit = 1 << (source & 0x1F);
    if let Some(reg) = destination {
        let result = cpu.registers.get_gpr(reg.value) | bit;
        cpu.registers.set_gpr(reg.value, result);
    } else {
        let result = cpu.registers.get_flags() | bit;
        cpu.registers.set_flags(result);
    }

    1
}

/// Executes the BCLR instruction.
fn bclr(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands().unwrap();

    // Extract the operands required to perform the operation.
    let destination = match insn.opcode() {
        0xF0 | 0xFD => operand!(operands[0], Operand::Register(reg) => reg),
        0xF4 | 0xF9 => None,
        _ => unreachable!(),
    };
    let source = match insn.opcode() {
        0xF0 => operand!(operands[1], Operand::I8(int) => int as u32).unwrap(),
        0xFD => operand!(operands[1], Operand::Register(reg) => cpu.registers.get_gpr(reg.value))
            .unwrap(),
        0xF4 => operand!(operands[0], Operand::I8(int) => int as u32).unwrap(),
        0xF9 => operand!(operands[0], Operand::Register(reg) => cpu.registers.get_gpr(reg.value))
            .unwrap(),
        _ => unreachable!(),
    };

    // Compute the result of the operation and store it.
    let bit = 1 << (source & 0x1F);
    if let Some(reg) = destination {
        let result = cpu.registers.get_gpr(reg.value) & !bit;
        cpu.registers.set_gpr(reg.value, result);
    } else {
        let result = cpu.registers.get_flags() & !bit;
        cpu.registers.set_flags(result);
    }

    1
}

/// Executes the BTGL instruction.
fn btgl(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands().unwrap();

    // Extract the operands required to perform the operation.
    let destination = match insn.opcode() {
        0xF0 | 0xFD => operand!(operands[0], Operand::Register(reg) => reg),
        0xF4 | 0xF9 => None,
        _ => unreachable!(),
    };
    let source = match insn.opcode() {
        0xF0 => operand!(operands[1], Operand::I8(int) => int as u32).unwrap(),
        0xFD => operand!(operands[1], Operand::Register(reg) => cpu.registers.get_gpr(reg.value))
            .unwrap(),
        0xF4 => operand!(operands[0], Operand::I8(int) => int as u32).unwrap(),
        0xF9 => operand!(operands[0], Operand::Register(reg) => cpu.registers.get_gpr(reg.value))
            .unwrap(),
        _ => unreachable!(),
    };

    // Compute the result of the operation and store it.
    let bit = 1 << (source & 0x1F);
    if let Some(reg) = destination {
        let result = cpu.registers.get_gpr(reg.value) ^ bit;
        cpu.registers.set_gpr(reg.value, result);
    } else {
        let result = cpu.registers.get_flags() ^ bit;
        cpu.registers.set_flags(result);
    }

    1
}

//! Helpers for frequently used instruction parsing routines to reduce boilerplate.

use enum_primitive::FromPrimitive;
use faucon_asm::{opcode::OperandSize, MemoryAccess, MemorySpace, Operand};

use super::{Cpu, CpuFlag};

/// Parses a [`faucon_asm::operands::MemoryAccess`] descriptor by composing the memory address
/// in question and extracting the corresponding [`faucon_asm::operands::MemorySpace`].
///
/// The data will be returned as a `(space, address)` tuple.
pub fn parse_memory_access(cpu: &Cpu, mem: Operand) -> Option<(MemorySpace, u32)> {
    if let Operand::Memory(mem) = mem {
        match mem {
            MemoryAccess::Reg { space, base } => Some((space, cpu.registers[base])),
            MemoryAccess::RegReg {
                space,
                base,
                offset,
                scale,
            } => Some((
                space,
                cpu.registers[base] + cpu.registers[offset] * scale as u32,
            )),
            MemoryAccess::RegImm {
                space,
                base,
                offset,
            } => Some((space, cpu.registers[base] + offset)),
        }
    } else {
        None
    }
}

/// Parses a CPU flag that is encoded in an operand.
pub fn parse_flag(flag: Operand) -> Option<CpuFlag> {
    if let Operand::Flag(imm) = flag {
        Some(CpuFlag::from_u8(imm).unwrap())
    } else {
        None
    }
}

/// Reads the value that is represented by an operand.
pub fn get_value(cpu: &Cpu, size: OperandSize, source: Operand) -> u32 {
    match source {
        Operand::Register(reg) => match size {
            OperandSize::EightBit => cpu.registers[reg] & 0xFF,
            OperandSize::SixteenBit => cpu.registers[reg] & 0xFFFF,
            OperandSize::ThirtyTwoBit | OperandSize::Unsized => cpu.registers[reg],
        },
        Operand::Immediate(imm) => imm as u32,
        Operand::UnsignedImmediate(imm) => imm,
        Operand::Memory(_) => read_mem(cpu, size, source),
        _ => panic!("The operand doesn't represent an extractable value"),
    }
}

/// Writes the value of a given source operand to a destination register.
pub fn write_reg(cpu: &mut Cpu, size: OperandSize, destination: Operand, source: Operand) {
    let value = get_value(cpu, size, source);
    match size {
        OperandSize::EightBit => cpu.registers[destination] &= !0xFF | value,
        OperandSize::SixteenBit => cpu.registers[destination] &= !0xFFFF | value,
        OperandSize::ThirtyTwoBit | OperandSize::Unsized => {
            cpu.registers[destination] &= !0xFFFFFFFF | value;
        }
    }
}

/// Writes a given value to a destination register.
pub fn write_value_to_reg(cpu: &mut Cpu, size: OperandSize, destination: Operand, source: u32) {
    match size {
        OperandSize::EightBit => cpu.registers[destination] &= !0xFF | (source & 0xFF),
        OperandSize::SixteenBit => cpu.registers[destination] &= !0xFFFF | (source & 0xFFFF),
        OperandSize::ThirtyTwoBit | OperandSize::Unsized => {
            cpu.registers[destination] &= !0xFFFFFFFF | source
        }
    }
}

/// Reads a value from the given [`faucon_asm::operands::MemoryAccess`] descriptor.
///
/// This is usually used by instruction handlers and should only be called when it
/// is guaranteed that the right [`faucon_asm::operands::Operand`] variant is being
/// passed.
pub fn read_mem(cpu: &Cpu, size: OperandSize, source: Operand) -> u32 {
    let (space, address) = parse_memory_access(cpu, source).unwrap();
    match space {
        MemorySpace::Io => read_imem(cpu, address),
        MemorySpace::DMem => read_dmem(cpu, size, address),
    }
}

/// Writes a given operand to a location in memory that is encoded in the destination operand.
pub fn write_mem(cpu: &mut Cpu, size: OperandSize, source: Operand, destination: Operand) {
    let (space, address) = parse_memory_access(cpu, destination).unwrap();
    match space {
        MemorySpace::Io => panic!("Unsupported IMem write access attempted"),
        MemorySpace::DMem => write_dmem(cpu, size, address, get_value(cpu, size, source)),
    };
}

fn read_imem(cpu: &Cpu, address: u32) -> u32 {
    cpu.memory.read_code_addr(address as u16)
}

fn read_dmem(cpu: &Cpu, size: OperandSize, address: u32) -> u32 {
    match size {
        OperandSize::EightBit => cpu.memory.read_data_byte(address) as u32,
        OperandSize::SixteenBit => cpu.memory.read_data_halfword(address) as u32,
        OperandSize::ThirtyTwoBit | OperandSize::Unsized => cpu.memory.read_data_word(address),
    }
}

fn write_dmem(cpu: &mut Cpu, size: OperandSize, address: u32, value: u32) {
    match size {
        OperandSize::EightBit => cpu.memory.write_data_byte(address, value as u8),
        OperandSize::SixteenBit => cpu.memory.write_data_halfword(address, value as u16),
        OperandSize::ThirtyTwoBit | OperandSize::Unsized => {
            cpu.memory.write_data_word(address, value)
        }
    }
}

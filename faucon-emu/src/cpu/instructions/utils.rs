//! Helpers for frequently used instruction parsing routines to reduce boilerplate.

use faucon_asm::{opcode::OperandSize, MemoryAccess, MemorySpace, Operand};

use super::Cpu;

/// Parses a [`MemoryAccess`] descriptor by composing the memory address in question and
/// extracting the corresponding [`MemorySpace`].
///
/// The data will be returned as a `(space, address)` tuple.
///
/// [`MemoryAccess`]: /faucon-asm/operands/enum.MemoryAccess.html
/// [`MemorySpace`]: /faucon-asm/operands/enum.MemorySpace.html
pub fn parse_memory_access(cpu: &mut Cpu, mem: Operand) -> (MemorySpace, u32) {
    if let Operand::Memory(mem) = mem {
        match mem {
            MemoryAccess::Reg { space, base } => (space, cpu.registers[base]),
            MemoryAccess::RegReg {
                space,
                base,
                offset,
                scale,
            } => (
                space,
                cpu.registers[base] + cpu.registers[offset] * scale as u32,
            ),
            MemoryAccess::RegImm {
                space,
                base,
                offset,
            } => (space, cpu.registers[base] + offset),
        }
    } else {
        panic!("Invalid operand supplied")
    }
}

/// Reads a value from a given location in Falcon memory space and stores it in a
/// destination register.
///
/// This is typically used by instructions and should only be used when the operands
/// are guaranteed to take the necessary variants for this function to succeed.
pub fn memory_to_register(cpu: &mut Cpu, size: OperandSize, mem: Operand, reg: Operand) {
    // Read a value from the requested memory space.
    let (space, address) = parse_memory_access(cpu, mem);
    let result = match space {
        MemorySpace::IMem => read_imem(cpu, address),
        MemorySpace::DMem => read_dmem(cpu, size, address),
    };

    // Write the result to the register.
    match size {
        OperandSize::ThirtyTwoBit | OperandSize::Unsized => cpu.registers[reg] = result,
        _ => cpu.registers[reg] |= result,
    };
}

/// Writes a value from a given source register to a desired location in Falcon memory
/// space.
///
/// This is typically used by instructions and should only be used when the operands
/// are guaranteed to take the necessary variants for this function to succeed.
pub fn register_to_memory(cpu: &mut Cpu, size: OperandSize, reg: Operand, mem: Operand) {
    // Extract the value from the register.
    let value = match size {
        OperandSize::EightBit => (cpu.registers[reg] & 0xFF) as u32,
        OperandSize::SixteenBit => (cpu.registers[reg] & 0xFFFF) as u32,
        OperandSize::ThirtyTwoBit | OperandSize::Unsized => (cpu.registers[reg] & 0xFFFFFFFF),
    };

    // Write it to memory.
    let (space, address) = parse_memory_access(cpu, mem);
    match space {
        MemorySpace::DMem => write_dmem(cpu, size, address, value),
        MemorySpace::IMem => panic!("Unsupported IMem write access attempted"),
    };
}

fn read_imem(cpu: &mut Cpu, address: u32) -> u32 {
    cpu.memory.read_code_addr(address as u16)
}

fn read_dmem(cpu: &mut Cpu, size: OperandSize, address: u32) -> u32 {
    match size {
        OperandSize::EightBit => (cpu.memory.read_data_byte(address) & 0xFF) as u32,
        OperandSize::SixteenBit => (cpu.memory.read_data_halfword(address) & 0xFFFF) as u32,
        OperandSize::ThirtyTwoBit | OperandSize::Unsized => {
            cpu.memory.read_data_word(address) & 0xFFFFFFFF
        }
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

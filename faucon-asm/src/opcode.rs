//! Abstractions over the Falcon opcode format.

use std::fmt;

/// Represents the operand size of an instruction.
///
/// The size is determined by the highest two bits of the first
/// instruction byte.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OperandSize {
    /// The instruction operates on operands of 8 bits in size.
    EightBit,
    /// The instruction operates on operands of 16 bits in size.
    SixteenBit,
    /// The instruction operates on operands of 32 bits in size.
    ThirtyTwoBit,
    /// The instruction does not do any operand sizing.
    ///
    /// This means that the instruction always operates on the full
    /// 32-bit operands.
    Unsized,
}

/// Extracts the operand size from a given opcode.
///
/// It denotes on what size an instruction operates.
pub const fn get_operand_size(opcode: u8) -> OperandSize {
    match opcode >> 6 {
        0b00 => OperandSize::EightBit,
        0b01 => OperandSize::SixteenBit,
        0b10 => OperandSize::ThirtyTwoBit,
        _ => OperandSize::Unsized, // Only possibility that is left is 3 anyway.
    }
}

impl From<u8> for OperandSize {
    fn from(opcode: u8) -> Self {
        get_operand_size(opcode)
    }
}

impl fmt::Display for OperandSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mnemonic = match self {
            OperandSize::EightBit => "b8 ",
            OperandSize::SixteenBit => "b16 ",
            OperandSize::ThirtyTwoBit => "b32 ",
            OperandSize::Unsized => "",
        };

        write!(f, "{}", mnemonic)
    }
}

/// Extracts the instruction form from a given opcode.
///
/// The returned tuple `(a, b)` contains the two parts of the opcode that
/// decide its form. If `a` is 0-2, then `b` decides on an instruction
/// within the form denoted by `a`. If `a` is 3, then `b` ultimately decides
/// on the form.
pub const fn get_opcode_form(opcode: u8) -> (u8, u8) {
    (opcode >> 4 & 0x3, opcode & 0xF)
}

//! Abstractions over the Falcon opcode format.

use std::fmt;

/// Represents the operand size of an instruction.
///
/// The size is determined by the highest two bits of the first
/// instruction byte.
#[derive(Clone, Debug, PartialEq)]
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

impl From<u8> for OperandSize {
    fn from(opcode: u8) -> Self {
        match opcode >> 6 {
            0b00 => OperandSize::EightBit,
            0b01 => OperandSize::SixteenBit,
            0b10 => OperandSize::ThirtyTwoBit,
            0b11 => OperandSize::Unsized,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for OperandSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mnemonic = match self {
            OperandSize::EightBit => "b8",
            OperandSize::SixteenBit => "b16",
            OperandSize::ThirtyTwoBit => "b32",
            OperandSize::Unsized => "",
        };

        write!(f, "{}", mnemonic)
    }
}

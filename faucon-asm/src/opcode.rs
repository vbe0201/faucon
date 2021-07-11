//! Utilities for working with different Falcon opcode formats.

use std::fmt;

/// Represents the operand size of an instruction.
///
/// The size is determined by the highest two bits of the first
/// instruction byte.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

impl OperandSize {
    /// Checks whether the operands are sized or not.
    pub const fn sized(&self) -> bool {
        !matches!(self, OperandSize::Unsized)
    }

    /// Gets the value of the bits that represents the operand size in in the
    /// instruction opcode.
    pub const fn value(&self) -> u8 {
        match self {
            OperandSize::EightBit => 0b00,
            OperandSize::SixteenBit => 0b01,
            OperandSize::ThirtyTwoBit => 0b10,
            OperandSize::Unsized => 0b11,
        }
    }
}

impl From<u8> for OperandSize {
    fn from(opcode: u8) -> Self {
        match opcode >> 6 {
            0b00 => OperandSize::EightBit,
            0b01 => OperandSize::SixteenBit,
            0b10 => OperandSize::ThirtyTwoBit,
            _ => OperandSize::Unsized, // 0b11 is the only fitting value here anyway.
        }
    }
}

impl fmt::Display for OperandSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mnemonic = match self {
            OperandSize::EightBit => ".b",
            OperandSize::SixteenBit => ".h",
            OperandSize::ThirtyTwoBit => ".w",
            OperandSize::Unsized => "",
        };

        write!(f, "{}", mnemonic)
    }
}

/// Extracts the instruction form from a given opcode.
///
/// The returned tuple `(a, b)` contains the two parts of the opcode that
/// decide its form. If `a` is 0-2, then `b` decides on a form within the
/// group denoted by `a`. If `a` is 3, then `b` ultimately decides on the
/// form.
pub const fn get_opcode_form(opcode: u8) -> (u8, u8) {
    (opcode >> 4 & 0x3, opcode & 0xF)
}

/// Builds the instruction opcode given its two form components.
///
/// See [`get_opcode_form`] for more details on `a` and `b`.
pub const fn build_opcode_form(a: u8, b: u8) -> u8 {
    (a & 0x3) << 4 | b & 0xF
}

/// The location where the subopcode is stored within the instruction bytes.
///
/// In Falcon assembly, opcodes generally span a variety of instructions, many
/// cases require an additional subopcode to identify instructions uniquely.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SubopcodeLocation {
    /// The subopcode is encoded in the high 2 bits of byte 0.
    OH,
    /// The subopcode is encoded in the low 4 bits of byte 0.
    O1,
    /// The subopcode is encoded in the low 4 bits of byte 1.
    O2,
    /// The subopcode is encoded in the low 6 bits of byte 1.
    OL,
    /// The subopcode is encoded in the low 4 bits of byte 2.
    O3,
    /// The subopcode is encoded in the low 4 bits of byte 4.
    O5,
}

impl SubopcodeLocation {
    /// Gets the subopcode location as a byteslice index.
    ///
    /// The subopcodes of instructions may be encoded in different places depending
    /// on its form. This tells you which byte of the instruction should contain it
    /// by virtually returning its index in a byteslice. This information can then
    /// be used to read as many bytes as needed to find the one holding the subopcode.
    pub const fn position(&self) -> u64 {
        match self {
            SubopcodeLocation::OH => 0,
            SubopcodeLocation::O1 => 0,
            SubopcodeLocation::O2 => 1,
            SubopcodeLocation::OL => 1,
            SubopcodeLocation::O3 => 2,
            SubopcodeLocation::O5 => 4,
        }
    }

    /// Gets the bitmask for masking out the subopcode from its position.
    pub const fn mask(&self) -> u8 {
        match self {
            SubopcodeLocation::OH => 0b11 << 6,
            SubopcodeLocation::O1
            | SubopcodeLocation::O2
            | SubopcodeLocation::O3
            | SubopcodeLocation::O5 => 0b1111,
            SubopcodeLocation::OL => 0b111111,
        }
    }

    /// Brings the subopcode value into shape for being encoded directly.
    pub fn build_value(&self, value: u8) -> u8 {
        match self {
            SubopcodeLocation::OH => value << 6,
            SubopcodeLocation::O1
            | SubopcodeLocation::O2
            | SubopcodeLocation::O3
            | SubopcodeLocation::O5 => value & 0xF,
            SubopcodeLocation::OL => value & 0x3F,
        }
    }

    /// Extracts the subopcode value from the instruction bytes.
    pub fn parse_value(&self, insn: &[u8]) -> u8 {
        match self {
            SubopcodeLocation::OH => insn[0] >> 6,
            SubopcodeLocation::O1 => insn[0] & 0xF,
            SubopcodeLocation::O2 => insn[1] & 0xF,
            SubopcodeLocation::OL => insn[1] & 0x3F,
            SubopcodeLocation::O3 => insn[2] & 0xF,
            SubopcodeLocation::O5 => insn[4] & 0xF,
        }
    }
}

/// Parses the [`SubopcodeLocation`] for the given opcode chunks.
pub const fn get_subopcode_location(size: u8, a: u8, b: u8) -> Option<SubopcodeLocation> {
    match (size, a, b) {
        // Sized opcodes (0x00 - 0xBF)
        (0x0..=0x2, 0x0, _) => Some(SubopcodeLocation::OH),
        (0x0..=0x2, 0x1..=0x2, _) => Some(SubopcodeLocation::O1),
        (0x0..=0x2, 0x3, 0x0..=0x1) => Some(SubopcodeLocation::O2),
        (0x0..=0x2, 0x3, 0x2) => Some(SubopcodeLocation::O1),
        (0x0..=0x2, 0x3, 0x3..=0x4) => Some(SubopcodeLocation::O2),
        (0x0..=0x2, 0x3, 0x5) => Some(SubopcodeLocation::O1),
        (0x0..=0x2, 0x3, 0x6..=0x7) => Some(SubopcodeLocation::O2),
        (0x0..=0x2, 0x3, 0x8) => Some(SubopcodeLocation::O5),
        (0x0..=0x2, 0x3, 0x9..=0xC) => Some(SubopcodeLocation::O3),
        (0x0..=0x2, 0x3, 0xD) => Some(SubopcodeLocation::O2),
        (0x0..=0x2, 0x3, 0xE) => Some(SubopcodeLocation::OH),
        (0x0..=0x2, 0x3, 0xF) => Some(SubopcodeLocation::O1),

        // Unsized opcodes (0xC0 - 0xFF)
        (0x3, 0x0..=0x2, _) => Some(SubopcodeLocation::O1),
        (0x3, 0x3, 0x0..=0x2) => Some(SubopcodeLocation::O2),
        (0x3, 0x3, 0x3) => Some(SubopcodeLocation::O1),
        (0x3, 0x3, 0x4..=0x5) => Some(SubopcodeLocation::OL),
        (0x3, 0x3, 0x6..=0x7) => Some(SubopcodeLocation::O1),
        (0x3, 0x3, 0x8..=0x9) => Some(SubopcodeLocation::O2),
        (0x3, 0x3, 0xA) => Some(SubopcodeLocation::O3),
        (0x3, 0x3, 0xB..=0xC) => Some(SubopcodeLocation::O2),
        (0x3, 0x3, 0xD..=0xF) => Some(SubopcodeLocation::O3),

        // Unknown/Invalid
        _ => None,
    }
}

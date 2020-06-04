//! Disassembler for the Falcon v5 (fuc5) ISA.

use std::fs::File;
use std::io::Read;
use std::path::Path;

/// Denotes the operand size of an instruction.
///
/// The value is determined by the highest two bits of
/// the first byte of each instruction.
#[derive(Debug)]
pub enum OperandSize {
    /// Denotes an operand size of 8-bit.
    EightBit,
    /// Denotes an operand size of 16-bit.
    SixteenBit,
    /// Denotes an operand size of 32-bit.
    ThirtyTwoBit,

    /// Denotes an unknown/variable operand size.
    Unsized,
}

impl From<u8> for OperandSize {
    fn from(first_byte: u8) -> Self {
        // Check the highest two bits.
        match first_byte >> 6 {
            0b00 => OperandSize::EightBit,
            0b01 => OperandSize::SixteenBit,
            0b10 => OperandSize::ThirtyTwoBit,
            0b11 => OperandSize::Unsized,
            _ => panic!("Invalid instruction hit!"),
        }
    }
}

/// Possible locations in where subopcodes are stored.
///
/// These vary from instruction to instruction.
pub enum SubopcodeLocation {
    /// Subopcode is stored in the low 4 bits of byte 0.
    O1,
    /// Subopcode is stored in the low 4 bits of byte 1.
    O2,
    /// Subopcode is stored in the low 6 bits of byte 1.
    OL,
    /// Subopcode is stored in the low 4 bits of byte 2.
    O3,
}

/// Extracts the subopcode from the opcode, given its location.
fn extract_subopcode(opcode: &[u8], location: SubopcodeLocation) -> u8 {
    match location {
        SubopcodeLocation::O1 => opcode[0] & 0xF,
        SubopcodeLocation::O2 => opcode[1] & 0xF,
        SubopcodeLocation::OL => opcode[1] & 0x3F,
        SubopcodeLocation::O3 => opcode[2] & 0xF,
    }
}

/// Reads the contents of a given binary file into a byte array.
pub fn read_binary<P: AsRef<Path>>(path: P) -> Box<[u8]> {
    let mut binary_buffer = Vec::new();

    let mut file = File::open(path)
        .expect("Failed to read the given binary! Please verify that its path is valid!");
    file.read_to_end(&mut binary_buffer).unwrap();

    binary_buffer.into_boxed_slice()
}

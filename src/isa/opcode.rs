use crate::{isa::OperandSize, utils::BitField};

pub const INSTRUCTION_SIZE: BitField = BitField::new(6, 8);

static SUBOPCODE_TABLE: [BitField; 0x100] = {
    use OperandSize::*;

    const OH: BitField = BitField::new(6, 8);
    const O1: BitField = BitField::new(0, 4);
    const O2: BitField = BitField::new(8, 12);
    const OL: BitField = BitField::new(8, 14);
    const O3: BitField = BitField::new(16, 20);
    const O5: BitField = BitField::new(32, 36);

    let mut table = [const { BitField::new_with_value(0) }; 0x100];

    let mut opcode = 0;
    while opcode < 0x100 {
        // Break each table index down into opcode components.
        let size = operand_size(opcode as u8);
        let a = (opcode >> 4) & 0b11;
        let b = opcode & 0b1111;

        // Populate each table entry with the matching subopcode.
        table[opcode] = match (size, a, b) {
            // Sized opcodes (0x00 - 0xBF)
            (EightBit | SixteenBit | ThirtyTwoBit, 0x0, _) => OH,
            (EightBit | SixteenBit | ThirtyTwoBit, 0x1..=0x2, _) => O1,
            (EightBit | SixteenBit | ThirtyTwoBit, 0x3, 0x0..=0x1) => O2,
            (EightBit | SixteenBit | ThirtyTwoBit, 0x3, 0x2) => O1,
            (EightBit | SixteenBit | ThirtyTwoBit, 0x3, 0x3..=0x4) => O2,
            (EightBit | SixteenBit | ThirtyTwoBit, 0x3, 0x5) => O1,
            (EightBit | SixteenBit | ThirtyTwoBit, 0x3, 0x6..=0x7) => O2,
            (EightBit | SixteenBit | ThirtyTwoBit, 0x3, 0x8) => O5,
            (EightBit | SixteenBit | ThirtyTwoBit, 0x3, 0x9..=0xC) => O3,
            (EightBit | SixteenBit | ThirtyTwoBit, 0x3, 0xD) => O2,
            (EightBit | SixteenBit | ThirtyTwoBit, 0x3, 0xE) => OH,
            (EightBit | SixteenBit | ThirtyTwoBit, 0x3, 0xF) => O1,

            // Unsized opcodes (0xC0 - 0xFF)
            (Unsized, 0x0..=0x2, _) => O1,
            (Unsized, 0x3, 0x0..=0x2) => O2,
            (Unsized, 0x3, 0x3) => O1,
            (Unsized, 0x3, 0x4..=0x5) => OL,
            (Unsized, 0x3, 0x6..=0x7) => O1,
            (Unsized, 0x3, 0x8..=0x9) => O2,
            (Unsized, 0x3, 0xA) => O3,
            (Unsized, 0x3, 0xB..=0xC) => O2,
            (Unsized, 0x3, 0xD..=0xF) => O3,

            _ => unreachable!(),
        };

        opcode += 1;
    }

    table
};

#[inline]
pub const fn classify_subopcode(opcode: u8) -> BitField {
    SUBOPCODE_TABLE[opcode as usize]
}

#[inline]
pub const fn operand_size(opcode: u8) -> OperandSize {
    match INSTRUCTION_SIZE.decode(opcode as u64) {
        0b00 => OperandSize::EightBit,
        0b01 => OperandSize::SixteenBit,
        0b10 => OperandSize::ThirtyTwoBit,
        _ => OperandSize::Unsized,
    }
}

#[inline]
pub const fn set_operand_size(size: OperandSize, opcode: u8) -> u8 {
    let size = match size {
        OperandSize::EightBit => 0b00,
        OperandSize::SixteenBit => 0b01,
        OperandSize::ThirtyTwoBit => 0b10,
        OperandSize::Unsized => 0b11,
    };

    INSTRUCTION_SIZE.encode(opcode as u64, size) as u8
}

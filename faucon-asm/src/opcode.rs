/// The location within an instruction where the subopcode is stored.
///
/// In Falcon Assembly, opcodes generally span a variety of instructions,
/// which is why the subopcode is important for uniquely identifying the
/// nature of an instruction.
pub enum SubopcodeLocation {
    /// The subopcode is encoded in the low 4 bits of byte 0.
    O1,
    /// The subopcode is encoded in the low 4 bits of byte 1.
    O2,
    /// The subopcode is encoded in the low 6 bits of byte 1.
    OL,
    /// The subopcode is encoded in the low 4 bits of byte 2.
    O3,
}

impl From<u8> for SubopcodeLocation {
    fn from(opcode: u8) -> Option<Self> {
        match opcode {
            // Sized opcodes
            0x00..=0x2F => Some(SubopcodeLocation::O1),
            0x30..=0x37 => Some(SubopcodeLocation::O2),
            0x38..=0x3C => Some(SubopcodeLocation::O3),
            0x3D => Some(SubopcodeLocation::O2),

            // Unsized opcodes
            0xC0..=0xEF => Some(SubopcodeLocation::O1),
            0xF0..=0xF2 => Some(SubopcodeLocation::O2),
            0xF4..=0xF5 => Some(SubopcodeLocation::OL),
            0xF8..=0xF9 => Some(SubopcodeLocation::O2),
            0xFA => Some(SubopcodeLocation::O3),
            0xFC => Some(SubopcodeLocation::O2),
            0xFD..=0xFF => Some(SubopcodeLocation::O3),

            // Unknown/Invalid
            _ => None,
        }
    }
}

/// Parses the subopcode from an instruction, given its location.
///
/// The subopcode is what identifies instructions uniquely, in
/// combination with the opcode. As a result of that,
/// `(opcode, subopcode)` is needed for instruction lookup.
pub fn parse_subopcode(insn: &[u8], location: SubopcodeLocation) -> u8 {
    match location {
        SubopcodeLocation::O1 => insn[0] & 0xF,
        SubopcodeLocation::O2 => insn[1] & 0xF,
        SubopcodeLocation::OL => insn[1] & 0x3F,
        SubopcodeLocation::O3 => insn[2] & 0xF,
    }
}

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

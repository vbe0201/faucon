//! Abstractions over Falcon Assembly operands.

/// Denotes the operand size of an instruction.
///
/// The value is determined by the highest two
/// bits of an opcode.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperandSize {
    /// An operand size of 8 bits.
    EightBit,
    /// An operand size of 16 bits.
    SixteenBit,
    /// An operand size of 32 bits.
    ThirtyTwoBit,
    /// An unsized operand size.
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

pub(crate) fn is_unsized(opcode: u8) -> bool {
    (opcode >> 6) == 0b11
}

impl Into<u32> for OperandSize {
    fn into(self) -> u32 {
        match self {
            OperandSize::EightBit => 8,
            OperandSize::SixteenBit => 16,
            OperandSize::ThirtyTwoBit | OperandSize::Unsized => 32,
        }
    }
}

/// Denotes the location of a register operand in an instruction.
///
/// In Falcon Assembly, register operands individually have information
/// on where they are stored and encoded associated with them per
/// instruction.
///
/// This is one of the key details a [`RegisterMeta`] object is
/// composed of, along with [`RegisterDirection`].
///
/// [`RegisterMeta`]: struct.RegisterMeta.html
/// [`RegisterDirection`]: enum.RegisterDirection.html
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegisterLocation {
    /// The register is encoded in the low 4 bits of byte 1.
    Low1,
    /// The register is encoded in the high 4 bits of byte 1.
    High1,
    /// The register is encoded in the high 4 bits of byte 2.
    High2,
}

/// The direction in which a register is used.
///
/// In Falcon Assembly, register operands individually have information
/// on whether they are being used as the source or destination provider
/// for the instruction operation associated with them per instruction.
///
/// This is one of the key details a [`RegisterMeta`] object is composed
/// of, along with [`RegisterLocation`].
///
/// [`RegisterMeta`]: struct.RegisterMeta.html
/// [`RegisterLocation`]: enum.RegisterLocation.html
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegisterDirection {
    /// The register is encoded as a source provider for the instruction
    /// operation.
    Source,
    /// The register is encoded as a destination provider for the
    /// instruction operation.
    Destination,
    /// The register is encoded as both, a source and a destination
    /// provider for the instruction operation.
    SourceDestination,
}

/// A structure holding meta information pertaining to a register [`Operand`].
///
/// Registers are encoded in the instruction and can be extracted and utilized
/// based on the information denoted by this type.
///
/// [`Operand`]: enum.Operand.html
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RegisterMeta(pub RegisterLocation, pub RegisterDirection);

/// Parses a register within an instruction, given its corresponding
/// [`RegisterMeta`] object.
///
/// Registers are encoded as numbers within a range of 0-15, denoting
/// the index of the register. The location where it is encoded can
/// be derived from the [`RegisterLocation`] member of the
/// [`RegisterMeta`].
///
/// [`RegisterMeta`]: struct.RegisterMeta.html
/// [`RegisterLocation`]: enum.RegisterLocation.html
pub fn parse_register(insn: &[u8], meta: &RegisterMeta) -> u8 {
    match meta.0 {
        RegisterLocation::Low1 => insn[1] & 0xF,
        RegisterLocation::High1 => insn[1] >> 4,
        RegisterLocation::High2 => insn[2] >> 4,
    }
}

/// Describes an operand in a Falcon Assembly instruction.
///
/// Operands can either be a register, in which case, the [`RegisterMeta`]
/// object carrying all the important details is exposed or a numeric type
/// in various sizes.
///
/// [`RegisterMeta`]: struct.RegisterMeta.html
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperandMeta {
    /// An encoded register operand.
    R(RegisterMeta),
    /// 8-bit immediate encoded in byte 2.
    I8,
    /// 16-bit immediate encoded in little-endian byteorder, starting
    /// from byte 2.
    I16,
    /// 24-bit immediate encoded in little-endian byteorder, starting
    /// from byte 2.
    I24,
    /// 32-bit immediate encoded in little-endian byteorder, starting
    /// from byte 2.
    I32,
}

impl OperandMeta {
    /// Gets the location where an opcode is stored in an array of instruction
    /// bytes.
    ///
    /// This function helps the parser to decide how many bytes to read for the
    /// particular instruction to cover all operands and also gives details about
    /// where their value can be obtained.
    pub fn location(&self) -> usize {
        match self {
            OperandMeta::R(meta) => match meta.0 {
                RegisterLocation::Low1 => 1,
                RegisterLocation::High1 => 1,
                RegisterLocation::High2 => 2,
            },
            OperandMeta::I8 => 2,
            OperandMeta::I16 => 2,
            OperandMeta::I24 => 2,
            OperandMeta::I32 => 2,
        }
    }

    /// Gets the size of the operand.
    ///
    /// The size indicates over how many bytes an operand spans and is used
    /// by the parser to determine the amount of bytes that are occupied by
    /// the operands of a particular instruction.
    pub fn size(&self) -> usize {
        match self {
            OperandMeta::R(_) => 1,
            OperandMeta::I8 => 1,
            OperandMeta::I16 => 2,
            OperandMeta::I24 => 3,
            OperandMeta::I32 => 4,
        }
    }
}

/// Gets the [`OperandMeta`] that corresponds to a given opcode.
///
/// The Falcon architecture follows strict rules on how operand
/// layout looks like corresponding to an opcode. Some opcodes
/// might imply that instructions have no operands at all.
///
/// [`OperandMeta`]: enum.OperandMeta.html
pub fn get_opcode_meta(opcode: u8) -> Option<Vec<OperandMeta>> {
    let notation = match opcode {
        // Sized opcodes
        0x00..=0x0F => Some("R2S, R1S, I8"),
        0x10..=0x1F => Some("R1D, R2S, I8"),
        0x20..=0x2F => Some("R1D, R2S, I16"),
        0x30 => Some("R2S, I8"),
        0x31 => Some("R2S, I16"),
        0x34 => Some("R2D, I8"),
        0x36 => Some("R2SD, I8"),
        0x37 => Some("R2SD, I16"),
        0x38 => Some("R2S, R1S"),
        0x39 => Some("R1D, R2S"),
        0x3A => Some("R2D, R1S"),
        0x3B => Some("R2SD, R1S"),
        0x3C => Some("R3D, R2S, R1S"),
        0x3D => Some("R2SD"),

        // Unsized opcodes
        0xC0..=0xCF => Some("R1D, R2S, I8"),
        0xD0..=0xDF => Some("R2S, R1S, I8"),
        0xE0..=0xEF => Some("R1D, R2S, I16"),
        0xF0 => Some("R2SD, I8"),
        0xF1 => Some("R2SD, I16"),
        0xF2 => Some("R2S, I8"),
        0xF3 => Some("I16"),
        0xF4 => Some("I8"),
        0xF5 => Some("I16"),
        0xF6..=0xF7 => Some("R2D, R1S, I8"),
        0xF8 => None,
        0xF9 => Some("R2S"),
        0xFA => Some("R2S, R1S"),
        0xFC => Some("R2D"),
        0xFD => Some("R2SD, R1S"),
        0xFE => Some("R1D, R2S"),
        0xFF => Some("R3D, R2S, R1S"),

        // Unknown/Invalid
        _ => None,
    };

    if let Some(operands) = notation {
        Some(
            operands
                .split(',')
                .map(|fmt| OperandMeta::from(fmt))
                .collect(),
        )
    } else {
        None
    }
}

impl From<&str> for OperandMeta {
    fn from(fmt: &str) -> Self {
        match fmt.trim() {
            "R1S" => OperandMeta::R(RegisterMeta(
                RegisterLocation::Low1,
                RegisterDirection::Source,
            )),
            "R1D" => OperandMeta::R(RegisterMeta(
                RegisterLocation::Low1,
                RegisterDirection::Destination,
            )),
            "R1SD" => OperandMeta::R(RegisterMeta(
                RegisterLocation::Low1,
                RegisterDirection::SourceDestination,
            )),
            "R2S" => OperandMeta::R(RegisterMeta(
                RegisterLocation::High1,
                RegisterDirection::Source,
            )),
            "R2D" => OperandMeta::R(RegisterMeta(
                RegisterLocation::High1,
                RegisterDirection::Destination,
            )),
            "R2SD" => OperandMeta::R(RegisterMeta(
                RegisterLocation::High1,
                RegisterDirection::SourceDestination,
            )),
            "R3S" => OperandMeta::R(RegisterMeta(
                RegisterLocation::High2,
                RegisterDirection::Source,
            )),
            "R3D" => OperandMeta::R(RegisterMeta(
                RegisterLocation::High2,
                RegisterDirection::Destination,
            )),
            "R3SD" => OperandMeta::R(RegisterMeta(
                RegisterLocation::High2,
                RegisterDirection::SourceDestination,
            )),
            "I8" => OperandMeta::I8,
            "I16" => OperandMeta::I16,
            "I24" => OperandMeta::I24,
            "I32" => OperandMeta::I32,
            _ => panic!("Cannot parse invalid operand notation"),
        }
    }
}

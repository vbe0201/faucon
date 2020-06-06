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
    ///
    /// NOTE: This notation refers to the operand bytes, which resolves,
    /// including the opcode, to the second byte of an instruction.
    Low1,
    /// The register is encoded in the high 4 bits of byte 1.
    ///
    /// NOTE: This notation refers to the operand bytes, which resolves,
    /// including the opcode, to the second byte of an instruction.
    High1,
    /// The register is encoded in the high 4 bits of byte 2.
    ///
    /// NOTE: This notation refers to the operand bytes, which resolves,
    /// including the opcode, to the third byte of an instruction.
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

/// An operand in a Falcon Assembly instruction.
///
/// Operands can either be a register, in which case, the [`RegisterMeta`]
/// object carrying all the important details is exposed or a numeric type
/// in various sizes.
///
/// [`RegisterMeta`]: struct.RegisterMeta.html
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operand {
    /// An encoded register operand.
    R(RegisterMeta),
    /// 8-bit immediate encoded in byte 2.
    ///
    /// NOTE: This notation refers to the operand bytes, which resolves,
    /// including the opcode, to the third byte of an instruction.
    I8,
    /// 16-bit immediate encoded in little-endian byteorder, starting
    /// from byte 2.
    ///
    /// NOTE: This notation refers to the operand bytes, which resolves,
    /// including the opcode, to the third byte of an instruction.
    I16,
}

impl Operand {
    /// Checks whether the operand is a register instead of an
    /// immediate.
    ///
    /// # Example
    ///
    /// ```
    /// use faucon_asm::operand::*;
    ///
    /// assert_eq!(
    ///     Operand::R(RegisterMeta(
    ///         RegisterLocation::Low1,
    ///         RegisterDirection::Source
    ///     ))
    ///     .is_register(),
    ///     true
    /// );
    /// ```
    pub fn is_register(&self) -> bool {
        !self.is_immediate()
    }

    /// Checks whether the operand is an immediate instead of a
    /// register.
    ///
    /// # Example
    ///
    /// ```
    /// use faucon_asm::operand::Operand;
    ///
    /// assert_eq!(Operand::I8.is_immediate(), true);
    /// ```
    pub fn is_immediate(&self) -> bool {
        match self {
            Operand::R(_) => false,
            _ => true,
        }
    }
}

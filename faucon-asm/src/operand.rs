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
    I8,
    /// 16-bit immediate encoded in little-endian byteorder, starting
    /// from byte 2.
    I16,
    /// 32-bit immediate encoded in little-endian byteorder, starting
    /// from byte 2.
    I32,
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

impl<'a> From<&'a str> for Operand {
    fn from(fmt: &'a str) -> Self {
        match fmt.trim() {
            "R1S" => Operand::R(RegisterMeta(
                RegisterLocation::Low1,
                RegisterDirection::Source,
            )),
            "R1D" => Operand::R(RegisterMeta(
                RegisterLocation::Low1,
                RegisterDirection::Destination,
            )),
            "R1SD" => Operand::R(RegisterMeta(
                RegisterLocation::Low1,
                RegisterDirection::SourceDestination,
            )),
            "R2S" => Operand::R(RegisterMeta(
                RegisterLocation::High1,
                RegisterDirection::Source,
            )),
            "R2D" => Operand::R(RegisterMeta(
                RegisterLocation::High1,
                RegisterDirection::Destination,
            )),
            "R2SD" => Operand::R(RegisterMeta(
                RegisterLocation::High1,
                RegisterDirection::SourceDestination,
            )),
            "R3S" => Operand::R(RegisterMeta(
                RegisterLocation::High2,
                RegisterDirection::Source,
            )),
            "R3D" => Operand::R(RegisterMeta(
                RegisterLocation::High2,
                RegisterDirection::Destination,
            )),
            "R3SD" => Operand::R(RegisterMeta(
                RegisterLocation::High2,
                RegisterDirection::SourceDestination,
            )),
            "I8" => Operand::I8,
            "I16" => Operand::I16,
            "I32" => Operand::I32,
            _ => panic!("Cannot parse invalid operand notation"),
        }
    }
}

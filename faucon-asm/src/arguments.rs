//! Arguments that are encoded in Falcon Assembly instructions.

use std::io::{Read, Write};

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use num_traits::{PrimInt, Signed, Unsigned};

/// An unsigned 8-bit immediate.
///
/// These are used for bit positions, shifts and 8-bit instructions.
pub const I8: Immediate<u8> = Immediate(2, 1, None, None);

/// An unsigned 8-bit immediate zero-extended to 16 bits.
///
/// These are used for SETHI and 16-bit instructions.
pub const I8ZX16: Immediate<u16> = Immediate(2, 1, None, None);

/// A signed 8-bit immediate sign-extended to 16 bits.
///
/// These are used for SETHI and 16-bit instructions.
pub const I8SX16: SignedImmediate<i16> = SignedImmediate(2, 1, None, None);

/// An unsigned 8-bit immediate zero-extended to 32 bits.
///
/// These are used for memory addressing and most 32-bit instructions.
pub const I8ZX32: Immediate<u32> = Immediate(2, 1, None, None);

/// A signed 8-bit immediate sign-extended to 32 bits.
///
/// These are used for memory addressing and most 32-bit instructions.
pub const I8SX32: SignedImmediate<i32> = SignedImmediate(2, 1, None, None);

/// An unsigned 8-bit immediate zero-extended to 32 bits and shifted
/// to left by one.
///
/// These are mainly used for memory addressing.
pub const I8ZX32S1: Immediate<u32> = Immediate(2, 1, Some(1), None);

/// An unsigned 8-bit immediate zero-extended to 32 bits and shifted
/// to left by two.
///
/// These are mainly used for memory addressing.
pub const I8ZX32S2: Immediate<u32> = Immediate(2, 1, Some(2), None);

/// An unsigned 16-bit immediate truncated to the low 8 bits.
///
/// Used by 8-bit instructions which have a 16-bit immediate form
/// for whatever reason.
pub const I16T8: Immediate<u16> = Immediate(2, 2, None, Some(0xFF));

/// An unsigned 16-bit immediate.
///
/// These are used by SETHI and 16-bit instructions.
pub const I16: Immediate<u16> = Immediate(2, 2, None, None);

/// An unsigned 16-bit immediate zero-extended to 32 bits.
///
/// These are used for most 32-bit instructions.
pub const I16ZX32: Immediate<u32> = Immediate(2, 2, None, None);

/// A signed 16-bit immediate sign-extended to 32 bits.
///
/// These are used for most 32-bit instructions.
pub const I16SX32: SignedImmediate<i32> = SignedImmediate(2, 2, None, None);

/// An unsigned 24-bit immediate zero-extended to 32 bits.
///
/// These are used for absolute call/jump addresses.
pub const I24: Immediate<u32> = Immediate(2, 3, None, None);

/// An argument that represents an instruction operand.
///
/// Arguments are a layer of abstraction between the nature
/// of an instruction and the actual data to extract or pack.
///
/// In practice, implementors of this trait act as parsing
/// interfaces for the underlying data structures and their
/// representation. They're intended to be flexible enough to
/// adapt to every special circumstance in instruction encoding
/// so that dirty hacks and alike can be avoided for ISA
/// inconsistencies across Falcon revisions.
pub trait Argument<T: PrimInt> {
    /// Reads the argument from the bytes echoed by the given
    /// [`Read`]er.
    ///
    /// [`Read`]: https://doc.rust-lang.org/std/io/trait.Read.html
    fn read<R: Read>(&self, reader: &mut R) -> T;

    /// The start of the operand in the instruction bytes.
    ///
    /// This method must be implemented for [`Argument::read`] to
    /// correctly determine where to start extracting the operand.
    ///
    /// [`Argument::read`]: trait.Argument.html#method.read
    fn start(&self) -> usize;

    /// The size of the operand in bytes.
    ///
    /// This method must be implemented for [`Argument::read`] to
    /// correctly determine in how many bytes the operand encoding
    /// takes.
    ///
    /// [`Argument::read`]: trait.Argument.html#method.read
    fn size(&self) -> usize;

    /// Returns a bitmask that needs to be applied to the operand
    /// to get its correct form.
    ///
    /// This method must be implemented for [`Argument::read`] to
    /// correctly unmask extracted operands.
    ///
    /// [`Argument::read`]: trait.Argument.html#method.read
    fn mask(&self) -> usize;

    /// Serializes the given value into its binary representation and
    /// writes it to the [`Write`]r.
    ///
    /// [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
    fn write<W: Write>(&self, value: T, writer: &mut W);
}

/// An immediate instruction operand, given as an unsized integer
/// value.
///
/// In cases where it is necessary, immediates will be zero-extended
/// to the necessary size.
///
/// The tuple arguments it carries are `(position, width, shift)`.
#[derive(Clone, Debug, PartialEq)]
pub struct Immediate<T: PrimInt + Unsigned>(usize, usize, Option<T>, Option<usize>);

impl Argument<u8> for Immediate<u8> {
    fn read<R: Read>(&self, reader: &mut R) -> u8 {
        let value = reader.read_u8().unwrap() & self.mask() as u8;

        value << self.2.unwrap_or(0)
    }

    fn start(&self) -> usize {
        self.0
    }

    fn size(&self) -> usize {
        self.1
    }

    fn mask(&self) -> usize {
        self.3.unwrap_or(0xFF)
    }

    fn write<W: Write>(&self, value: u8, writer: &mut W) {
        writer.write_u8(value >> self.2.unwrap_or(0)).unwrap();
    }
}

impl Argument<u16> for Immediate<u16> {
    fn read<R: Read>(&self, reader: &mut R) -> u16 {
        let mut value = match self.size() {
            1 => reader.read_u8().unwrap() as u16,
            2 => reader.read_u16::<LittleEndian>().unwrap(),
            _ => panic!("Unsupported size argument supplied"),
        };
        value &= self.mask() as u16;

        value << self.2.unwrap_or(0)
    }

    fn start(&self) -> usize {
        self.0
    }

    fn size(&self) -> usize {
        self.1
    }

    fn mask(&self) -> usize {
        let value = match self.size() {
            1 => 0xFF,
            2 => 0xFFFF,
            _ => panic!("Unsupported size argument supplied"),
        };

        self.3.unwrap_or(value)
    }

    fn write<W: Write>(&self, value: u16, writer: &mut W) {
        let value = value >> self.2.unwrap_or(0);
        match self.size() {
            1 => writer.write_u8(value as u8).unwrap(),
            2 => writer.write_u16::<LittleEndian>(value).unwrap(),
            _ => panic!("Unsupported size argument supplied"),
        };
    }
}

impl Argument<u32> for Immediate<u32> {
    fn read<R: Read>(&self, reader: &mut R) -> u32 {
        let mut value = match self.size() {
            1 => reader.read_u8().unwrap() as u32,
            2 => reader.read_u16::<LittleEndian>().unwrap() as u32,
            3 => reader.read_u24::<LittleEndian>().unwrap(),
            4 => reader.read_u32::<LittleEndian>().unwrap(),
            _ => panic!("Unsupported size argument supplied"),
        };
        value &= self.mask() as u32;

        value << self.2.unwrap_or(0)
    }

    fn start(&self) -> usize {
        self.0
    }

    fn size(&self) -> usize {
        self.1
    }

    fn mask(&self) -> usize {
        let value = match self.size() {
            1 => 0xFF,
            2 => 0xFFFF,
            3 => 0xFFFFFF,
            4 => 0xFFFFFFFF,
            _ => panic!("Unsupported size argument supplied"),
        };

        self.3.unwrap_or(value)
    }

    fn write<W: Write>(&self, value: u32, writer: &mut W) {
        let value = value >> self.2.unwrap_or(0);
        match self.size() {
            1 => writer.write_u8(value as u8).unwrap(),
            2 => writer.write_u16::<LittleEndian>(value as u16).unwrap(),
            3 => writer.write_u24::<LittleEndian>(value).unwrap(),
            4 => writer.write_u32::<LittleEndian>(value).unwrap(),
            _ => panic!("Unsupported size argument supplied"),
        };
    }
}

/// An immediate instruction operand, given as a signed integer
/// value.
///
/// In cases where it is necessary, immediates will be sign-extended
/// to the necessary size.
///
/// The tuple arguments it carries are `(position, width, shift)`.
#[derive(Clone, Debug, PartialEq)]
pub struct SignedImmediate<T: PrimInt + Signed>(usize, usize, Option<T>, Option<usize>);

impl Argument<i8> for SignedImmediate<i8> {
    fn read<R: Read>(&self, reader: &mut R) -> i8 {
        let value = reader.read_i8().unwrap() & self.mask() as i8;

        value << self.2.unwrap_or(0)
    }

    fn start(&self) -> usize {
        self.0
    }

    fn size(&self) -> usize {
        self.1
    }

    fn mask(&self) -> usize {
        self.3.unwrap_or(0xFF)
    }

    fn write<W: Write>(&self, value: i8, writer: &mut W) {
        writer.write_i8(value >> self.2.unwrap_or(0)).unwrap();
    }
}

impl Argument<i16> for SignedImmediate<i16> {
    fn read<R: Read>(&self, reader: &mut R) -> i16 {
        let mut value = match self.size() {
            1 => reader.read_i8().unwrap() as i16,
            2 => reader.read_i16::<LittleEndian>().unwrap(),
            _ => panic!("Unsupported size argument supplied"),
        };
        value &= self.mask() as i16;

        value << self.2.unwrap_or(0)
    }

    fn start(&self) -> usize {
        self.0
    }

    fn size(&self) -> usize {
        self.1
    }

    fn mask(&self) -> usize {
        let value = match self.size() {
            1 => 0xFF,
            2 => 0xFFFF,
            _ => panic!("Unsupported size argument supplied"),
        };

        self.3.unwrap_or(value)
    }

    fn write<W: Write>(&self, value: i16, writer: &mut W) {
        let value = value >> self.2.unwrap_or(0);
        match self.size() {
            1 => writer.write_i8(value as i8).unwrap(),
            2 => writer.write_i16::<LittleEndian>(value).unwrap(),
            _ => panic!("Unsupported size argument supplied"),
        };
    }
}

impl Argument<i32> for SignedImmediate<i32> {
    fn read<R: Read>(&self, reader: &mut R) -> i32 {
        let mut value = match self.size() {
            1 => reader.read_i8().unwrap() as i32,
            2 => reader.read_i16::<LittleEndian>().unwrap() as i32,
            3 => reader.read_i24::<LittleEndian>().unwrap(),
            4 => reader.read_i32::<LittleEndian>().unwrap(),
            _ => panic!("Unsupported size argument supplied"),
        };
        value &= self.mask() as i32;

        value << self.2.unwrap_or(0)
    }

    fn start(&self) -> usize {
        self.0
    }

    fn size(&self) -> usize {
        self.1
    }

    fn mask(&self) -> usize {
        let value = match self.size() {
            1 => 0xFF,
            2 => 0xFFFF,
            3 => 0xFFFFFF,
            4 => 0xFFFFFFFF,
            _ => panic!("Unsupported size argument supplied"),
        };

        self.3.unwrap_or(value)
    }

    fn write<W: Write>(&self, value: i32, writer: &mut W) {
        let value = value >> self.2.unwrap_or(0);
        match self.size() {
            1 => writer.write_i8(value as i8).unwrap(),
            2 => writer.write_i16::<LittleEndian>(value as i16).unwrap(),
            3 => writer.write_i24::<LittleEndian>(value).unwrap(),
            4 => writer.write_i32::<LittleEndian>(value).unwrap(),
            _ => panic!("Unsupported size argument supplied"),
        };
    }
}

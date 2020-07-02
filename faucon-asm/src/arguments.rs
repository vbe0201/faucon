//! Arguments that are encoded in Falcon Assembly instructions.

use std::io::{Read, Write};

use byteorder::{ByteOrder, LittleEndian};
use num_traits::PrimInt;

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
    /// Parses the argument from the bytes echoed by the given
    /// [`Read`]er.
    ///
    /// [`Read`]: https://doc.rust-lang.org/std/io/trait.Read.html
    fn parse<R: Read>(&self, data: &mut R) -> T;

    /// The start of the operand in the instruction bytes.
    ///
    /// This method must be implemented for [`Argument::parse`] to
    /// correctly determine where to start extracting the operand.
    ///
    /// [`Argument::parse`]: trait.Argument.html#method.parse
    fn start(&self) -> usize;

    /// The size of the operand in bytes.
    ///
    /// This method must be implemented for [`Argument::parse`] to
    /// correctly determine in how many bytes the operand encoding
    /// takes.
    ///
    /// [`Argument::parse`]: trait.Argument.html#method.parse
    fn size(&self) -> usize;

    /// Returns a bitmask that needs to be applied to the operand
    /// to get its correct form.
    ///
    /// This method must be implemented for [`Argument::parse`] to
    /// correctly unmask extracted operands.
    fn mask(&self) -> usize;

    /// Serializes the given value into its binary representation and
    /// writes it to the [`Write`]r.
    ///
    /// [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
    fn write<W: Write>(&self, value: T, writer: &mut W);
}

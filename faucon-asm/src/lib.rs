pub mod arguments;
pub mod disassembler;
pub mod isa;
pub mod opcode;

/// A result that is returned by the functions in this crate.
pub type Result<T, E = Error> = std::result::Result<T, E>;

/// Errors that are utilized by the crate.
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// An error that occurs when the opcode corresponding to an instruction
    /// cannot be identified.
    ///
    /// In such a case, this variant holds the opcode byte in question.
    UnknownInstruction(u8),
    /// An I/O error has occurred while reading data from a stream.
    IoError,
    /// An EOF has been reached while streaming a file through [`Read`].
    ///
    /// [`Read`]: https://doc.rust-lang.org/std/io/trait.Read.html
    Eof,
}

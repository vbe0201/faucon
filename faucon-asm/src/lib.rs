use std::fmt;

mod arguments;
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

/// A Falcon processor instruction.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Instruction {
    bytes: Vec<u8>,
    /// The operand size of the instruction.
    pub operand_size: opcode::OperandSize,
    meta: isa::InstructionMeta,
}

impl Instruction {
    /// Constructs a new instruction from its byte representation and metadata.
    pub fn new(
        bytes: Vec<u8>,
        operand_size: opcode::OperandSize,
        meta: isa::InstructionMeta,
    ) -> Self {
        // TODO: InstructionKind::XXX?

        Instruction {
            bytes,
            operand_size,
            meta,
        }
    }

    /// Gets the [`InstructionKind`] that is represented by this instruction variant.
    ///
    /// [`InstructionKind`]: ./isa/enum.InstructionKind.html
    pub fn kind(&self) -> isa::InstructionKind {
        self.meta.kind
    }

    /// Gets the length of an instruction by counting its bytes.
    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    /// Constructs the opcode of the instruction.
    ///
    /// The opcode is traditionally the first instruction byte. For unsized instructions,
    /// the high two bits (`0b11`) are relevant, for sized instructions, they must be
    /// masked out.
    pub fn opcode(&self) -> u8 {
        match self.operand_size {
            opcode::OperandSize::Unsized => self.bytes[0],
            _ => self.bytes[0] & !0xC0,
        }
    }

    /// Gets the subopcode of the instruction.
    ///
    /// The subopcode is used to identify instructions uniquely within a specific form,
    /// whenever needed.
    pub fn subopcode(&self) -> u8 {
        self.meta.subopcode
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Pretty-print operands as well.
        write!(f, "{} {}", self.kind(), self.operand_size)
    }
}

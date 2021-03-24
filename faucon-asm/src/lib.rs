//! Rust library for processing NVIDIA Falcon assembly.
//!
//! # About the Falcon
//!
//! The **Fa**st **L**ogic **Con**troller is a series of general-purpose embedded
//! microprocessors that have been produced since around 2005. With over three
//! billion units shipped, the Falcon has been used in many different places and
//! platforms, primarily NVIDIA GPUs starting from G98, but really everywhere a
//! controller for logic processing was needed.
//!
//! Falcon units consist of:
//!
//! - the core processor with its SRAM for code and data
//! - the I/O space for communication with host systems
//! - FIFO interface logic (optional)
//! - memory interface logic (optional)
//! - cryptographic AES coprocessor, making the Falcon a "secretful unit" (optional)
//! - unit-specific logic to control, depending on how the processor is used (optional)
//!
//! # The Instruction Set Architecture
//!
//! The inner workings of Falcon assembly are documented elsewhere. If you see this
//! disclaimer, odds are pretty high that nobody has started to work on it yet.
//!
//! The heart of this crate is the [`Instruction`] structure. Objects should never
//! be created manually, unless you know what you are doing. Rather, instances
//! should be obtained through the [`read_instruction`] function, which disassembles
//! a chunk of binary data into Falcon assembly.
//!
//! See the respective documentation for more details on the usage possibilities.
//!
//! ## Pretty-printing instructions
//!
//! Instructions implement the [`Display`] trait so they can emit valid assembly code
//! for the wrapped instruction which could be thrown at an assembler.
//!
//! ```
//! let instruction = faucon_asm::read_instruction(&mut &[0xBFu8, 0x1Fu8][..])
//!     .expect("Failed to disassemble the given bytes into a valid instruction");
//!
//! assert_eq!(instruction.to_string(), "ld b32 $r15 D[$r1]");
//! ```
//!
//! ## Instruction operands
//!
//! Of course, an [`Instruction`] object lets you access its operands which are used to
//! execute the operation. There are various types of operands in Falcon assembly:
//!
//! - registers (`$r0`, `$sp`, ...)
//! - immediates (`0xAB`, `-0x98`, ...)
//! - CPU flags from the `$flags` register (`pX`, `c`, ...)
//! - direct memory accesses (`D[$sp + 0xAB]`, `I[$r0 + $r4]`, ...)
//!
//! To work with these data efficiently, Falcon wraps up the values and corresponding
//! metadata in the [`Operand`] enumeration. A list of instruction operands can be
//! obtained through [`Instruction::operands`].
//!
//! ## Comparing instructions
//!
//! In Falcon assembly, it is quite usual that [`Instruction`]s have multiple variants
//! with different opcodes and different operand combinations. To compare the natures
//! of instructions, [`Instruction::kind`] exposes an [`InstructionKind`] variant.
//!
//! ```
//! let instruction = faucon_asm::read_instruction(&mut &[0xBFu8, 0x1Fu8][..])
//!     .expect("Failed to disassemble the given bytes into a valid instruction");
//!
//! assert_eq!(instruction.kind(), faucon_asm::InstructionKind::LD);
//! ```
//!
//! # Assembling instructions
//!
//! Functionality for assembling intermediate representation to machine code is
//! currently unsupported and planned for the future.
//!
//! For the time being, it is advised to use `envyas` from the [envytools]
//! collection.
//!
//! # Disassembling instructions
//!
//! As mentioned previously, the [`read_instruction`] can be used to disassemble
//! raw instruction bytes into [`Instruction`] objects. The function can be called
//! repeatedly on a buffer of code until an error or [`Error::Eof`] occurs.
//!
//! It is within the user's responsibility to ensure that all possible exceptions
//! are handled correctly.
//!
//! [`Instruction`]: struct.Instruction.html
//! [`read_instruction`]: fn.read_instruction.html
//! [`Display`]: https://doc.rust-lang.org/std/fmt/trait.Display.html
//! [`Operand`]: ./operands/enum.Operand.html
//! [`Instruction::operands`]: struct.Instruction.html#method.operands
//! [`Instruction::kind`]: struct.Instruction.html#method.kind
//! [`InstructionKind`]: ./isa/enum.InstructionKind.html
//! [envytools]: https://github.com/envytools/envytools
//! [`Error::Eof`]: enum.Error.html#variant.Eof

#![feature(const_unreachable_unchecked)]

mod arguments;
pub mod assembler;
mod bytes_ext;
pub mod disassembler;
pub mod isa;
pub mod opcode;
pub mod operands;

use std::fmt;
use std::io;

pub use assembler::*;
pub use disassembler::*;
pub use isa::InstructionKind;
pub use opcode::OperandSize;
use opcode::*;
pub use operands::*;

/// A result that is returned by the functions in this crate.
// TODO: Get rid of this soon.
pub type Result<T, E = FalconError> = std::result::Result<T, E>;

/// Error kinds that may occur when assembling or disassembling code
/// using this crate.
#[derive(Debug)]
pub enum FalconError {
    /// An opcode cannot be identified as a valid instruction during
    /// disassembling machine code.
    ///
    /// In such a case, this variant holds the opcode byte in question.
    InvalidOpcode(u8),
    /// The assembler failed to parse the input source to build machine
    /// code out of it.
    ///
    /// Encapsulates the original [`ParseError`] object.
    ParseError(ParseError),
    /// An I/O error has occurred while reading data from an input source.
    ///
    /// Encapsulates the original [`std::io::Error`] object.
    IoError(io::Error),
    /// An EOF has been prematurely reached while streaming a file through
    /// [`std::io::Read`].
    ///
    /// This differs from [`std::io::ErrorKind::UnexpectedEof`] in that regard
    /// that this error only occurs when more data were semantically expected
    /// for an operation to successfully complete.
    Eof,
}

impl fmt::Display for FalconError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FalconError::InvalidOpcode(op) => {
                write!(f, "Invalid opcode encountered: {:#X}", op)
            }
            FalconError::ParseError(e) => write!(f, "{}", e),
            FalconError::IoError(e) => write!(f, "{}", e),
            FalconError::Eof => write!(f, "Unexpected EOF encountered while trying to read a file"),
        }
    }
}

impl std::error::Error for FalconError {}

/// A Falcon processor instruction.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Instruction {
    meta: isa::InstructionMeta,
    operand_size: OperandSize,
    operands: Vec<Operand>,
    pc: u32,

    raw_bytes: Option<Vec<u8>>,
}

impl Instruction {
    /// Constructs a new instruction from its byte representation and metadata.
    pub fn new(
        meta: isa::InstructionMeta,
        mut operand_size: OperandSize,
        operands: Vec<Operand>,
        pc: u32,
    ) -> Self {
        // Certain Falcon weirdos encode their subopcode in the high size bits and thus
        // making the instruction per se unsized. We need to make sure to not use a false
        // positive operand size.
        if get_subopcode_location(operand_size.value(), meta.a, meta.b)
            == Some(SubopcodeLocation::OH)
        {
            operand_size = OperandSize::Unsized;
        }

        Instruction {
            meta,
            operand_size,
            operands,
            pc,
            raw_bytes: None,
        }
    }

    /// Assigns a vector of raw instruction bytes to this instruction.
    ///
    /// If set to a value, this will be used as the return value of [`Instruction::assemble`]
    /// over assembling the instruction from its metadata from scratch.
    pub fn with_raw_bytes(mut self, bytes: Vec<u8>) -> Self {
        self.raw_bytes = Some(bytes);
        self
    }

    /// Provides immutable access to the raw bytes of this instruction, if possible.
    ///
    ///  This method usually returns `None` if the instruction was not obtained
    /// through the disassembler.
    pub fn raw_bytes(&self) -> Option<&Vec<u8>> {
        self.raw_bytes.as_ref()
    }

    /// Gets the value of the program counter at which the instruction lives.
    ///
    /// This references an address in memory that is relative to a base address,
    /// e.g. the address at which the program code is mapped.
    pub fn program_counter(&self) -> u32 {
        self.pc
    }

    /// Gets the [`InstructionKind`] that is represented by this instruction variant.
    pub fn kind(&self) -> isa::InstructionKind {
        self.meta.kind
    }

    /// Constructs the opcode of the instruction.
    ///
    /// The opcode is traditionally the first instruction byte. The high two bits either
    /// encode the operand sizing or a subopcode, so this is not relevant to the
    /// opcode and will not be masked in.
    pub fn opcode(&self) -> u8 {
        build_opcode_form(self.meta.a, self.meta.b)
    }

    /// Gets the subopcode of the instruction.
    ///
    /// The subopcode is used to identify instructions uniquely within a specific form,
    /// when needed.
    pub fn subopcode(&self) -> u8 {
        self.meta.subopcode
    }

    /// Gets the [`OperandSize`] of the instruction.
    ///
    /// The operand size determines which quantity of size in the operands is modified
    /// by the instruction. Sized instructions may choose between 8-bit, 16-bit and 32-bit
    /// variants, whereas unsized instructions always operate on the full 32 bits.
    pub fn operand_size(&self) -> OperandSize {
        self.operand_size
    }

    /// Gets a vector of instruction [`Operand`]s.
    pub fn operands(&self) -> &Vec<Operand> {
        &self.operands
    }

    /// Assembles the instruction into its machine code representation and writes the
    /// code to `output`.
    pub fn assemble(self, output: &mut Vec<u8>) {
        if let Some(bytes) = self.raw_bytes {
            output.extend(bytes);
        } else {
            todo!()
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.kind(), self.operand_size)?;
        for operand in self.operands() {
            write!(f, " {}", operand)?;
        }

        Ok(())
    }
}

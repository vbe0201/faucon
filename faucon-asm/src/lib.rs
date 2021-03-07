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

pub use disassembler::*;
pub use isa::InstructionKind;
pub use opcode::OperandSize;
use opcode::*;
pub use operands::*;

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
    offset: usize,
    operand_size: OperandSize,
    meta: isa::InstructionMeta,
}

impl Instruction {
    /// Constructs a new instruction from its byte representation and metadata.
    pub fn new(
        bytes: Vec<u8>,
        offset: usize,
        mut operand_size: OperandSize,
        meta: isa::InstructionMeta,
    ) -> Self {
        // Certain Falcon weirdos encode their subopcode in the high size bits and thus
        // making the instruction per se unsized. We need to make sure to not use a false
        // positive operand size.
        let (a, b) = get_opcode_form(bytes[0]);
        if get_subopcode_location(operand_size.value(), a, b) == Some(SubopcodeLocation::OH) {
            operand_size = OperandSize::Unsized;
        }

        Instruction {
            bytes,
            offset,
            operand_size,
            meta,
        }
    }

    /// Returns a reference to the raw byte representation of
    /// this `Instruction`.
    pub fn raw_bytes(&self) -> &[u8] {
        self.bytes.as_slice()
    }

    /// The offset of the instruction in memory.
    ///
    /// Depending on where the disassembler started reading the instruction, one must
    /// add this to the base address to get the real memory address of the instruction.
    pub fn memory_offset(&self) -> usize {
        self.offset
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
            OperandSize::Unsized => self.bytes[0],
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

    /// Gets the [`OperandSize`] of the instruction.
    ///
    /// The operand size determines which quantity of size in the operands is modified
    /// by the instruction. Sized instructions may choose between 8-bit, 16-bit and 32-bit
    /// variants, whereas unsized instructions always operate on the full 32 bits.
    ///
    /// [`OperandSize`]: ./opcode/enum.OperandSize.html
    pub fn operand_size(&self) -> OperandSize {
        self.operand_size
    }

    /// A vector of instruction [`Operand`]s.
    ///
    /// [`Operand`]: ./operands/enum.Operand.html
    pub fn operands(&self) -> Vec<Operand> {
        let mut operands = Vec::new();

        for arg in self.meta.operands.iter() {
            if let Some(arg) = arg {
                // Extract the real value of the operand from the instruction bytes.
                operands.push(Operand::parse(arg, &self.bytes));
            }
        }

        operands
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

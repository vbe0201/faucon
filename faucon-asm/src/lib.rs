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
//! The heart of this crate is the [`Instruction`] structure. Objects of it shall never
//! be created manually, unless you know what you are doing. Rather, instances may be
//! obtained through the [`read_instruction`] function, which disassembles a chunk of
//! binary data into Falcon assembly.
//!
//! See the respective documentation for more details on the usage possibilities.
//!
//! ## Pretty-printing instructions
//!
//! Instructions implement the [`std::fmt::Display`] trait so they can emit valid assembly
//! code for the wrapped instruction which could be thrown at an assembler.
//!
//! ```
//! let instruction = unsafe {
//!     faucon_asm::read_instruction(&mut &[0xBF, 0x1F][..], &mut 0).unwrap()
//! };
//!
//! assert_eq!(instruction.to_string(), "ld.w $r15 D[$r1]");
//! ```
//!
//! ## Instruction operands
//!
//! Of course, an [`Instruction`] object lets you access its operands which are used to
//! execute the operation. The various different types of operands are portrayed by the
//! [`Operand`] enumeration. A list of instruction operands can be obtained through
//! [`Instruction::operands`].
//!
//! # Assembling instructions
//!
//! Assembling instructions is supported and can be done either by using the [`Assembler`]
//! structure to build machine code out of source files or a raw string of assembly or by
//! calling [`Instruction::assemble`] on an instruction instance.
//!
//! Latter is discouraged when instructions were hand-constructed because a lot can go wrong
//! and no sanitization is done on the opcodes or the underlying encoding form.
//!
//! When assembling raw strings, a helper macro [`faucon_asm!`] is provided to construct
//! an instance of the [`Assembler`] and consume a `&str` of assembly language into a `Vec<u8>`
//! of machine code:
//!
//! ```
//! use faucon_asm::faucon_asm;
//!
//! let code = faucon_asm!("ld.w $r12 D[$r3+0x74];").expect("Failed to assemble the code");
//! assert_eq!(code, vec![0x98, 0x3C, 0x1D]);
//! ```
//!
//! # Disassembling instructions
//!
//! As mentioned previously, the [`read_instruction`] function should be used to
//! disassemble raw instruction bytes into [`Instruction`] objects. The function
//! can be called repeatedly on a buffer of code until an error or [`FalconError::Eof`]
//! occurs.
//!
//! However if a user wishes to pretty-print a sequence of instructions to a special
//! output sink, the [`Disassembler`] structure may be used which provides additional
//! formatting capabilities which include the current program counter of an instruction
//! and its representation in raw bytes.

#![deny(rust_2018_idioms, broken_intra_doc_links)]
#![feature(const_option)]

//pub mod assembler;
mod bitfields;
mod bit_utils;
pub mod disassembler;
pub mod isa;
pub mod opcode;
pub mod operands;

use std::fmt;
use std::io;

//use arguments::{Argument, MachineEncoding, Positional};
//pub use assembler::*;
pub use disassembler::*;
pub use isa::InstructionKind;
pub use opcode::OperandSize;
use opcode::*;
pub use operands::*;
/*
/// Assembles Falcon assembly to machine code at runtime.
///
/// This is mainly laid out for convenient usage of the assembler interface without
/// having to write any boilerplate code. Under the hood, it calls the
/// [`Assembler::assemble_str`] method and yields its return value.
///
/// No include path will be set up, so the `include` directive cannot be used inside
/// the code supplied to this macro.
///
/// # Example
///
/// ```
/// use faucon_asm::faucon_asm;
///
/// let result = faucon_asm!("halt;").expect("Failed to assemble the code");
/// assert_eq!(result, vec![0xF8, 0x02]);
/// ```
#[macro_export]
macro_rules! faucon_asm {
    ($asm:expr) => {
        $crate::Assembler::new().assemble_str($asm, &::std::ffi::OsString::from("<main>"))
    };
}*/

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
    //ParseError(ParseError),
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
            //FalconError::ParseError(e) => write!(f, "{}", e),
            FalconError::IoError(e) => write!(f, "{}", e),
            FalconError::Eof => write!(f, "Unexpected EOF encountered while trying to read a file"),
        }
    }
}

impl std::error::Error for FalconError {}

/// A Falcon processor instruction.
///
/// This is designed as a wrapper around a single Falcon assembly instruction that
/// conveniently lets users query metadata, operand values and encoding information
/// from it.
///
/// The easiest and recommended method for obtaining an instruction object is
/// [`crate::disassembler::read_instruction`]. Thus, it is generally assumed that
/// [`Instruction`]s more commonly appear in a disassembler rather than an assembler
/// context, although hand-construction of instructions is possible.
///
/// # Safety
///
/// An [`Instruction`] does not enforce any scrutiny on the data it encapsulates and
/// thus all means of obtaining an object of it are considered `unsafe`. See
/// [`Instruction::new`] for more thoughts on why this decision was made.
#[derive(Clone, PartialEq, Eq)]
pub struct Instruction {
    meta: isa::InstructionMeta,
    operand_size: OperandSize,
    operands: Vec<Operand>,
    pc: u32,

    raw_bytes: Vec<u8>,
}

impl Instruction {
    /// Constructs a new instruction from its metadata and operand values.
    ///
    /// # Safety
    ///
    /// To avoid unexpected side effects when working with [`Instruction`]s,
    /// make sure to provide valid data when constructing them manually.
    ///
    /// Although this would not trigger undefined behavior per se, it may
    /// result in undefined behavior in conjunction with [`Instruction::assemble`]
    /// producing malformed code that is feeded into a real Falcon unit.
    pub(crate) fn new(
        meta: isa::InstructionMeta,
        operand_size: OperandSize,
        operands: Vec<Operand>,
        pc: u32,
        raw_bytes: Vec<u8>,
    ) -> Self {
        Instruction {
            meta,
            operand_size,
            operands,
            pc,
            raw_bytes,
        }
    }

    /// Provides immutable access to the raw bytes of the instruction.
    pub fn raw_bytes(&self) -> &[u8] {
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
        if self.meta.sized {
            self.operand_size
        } else {
            OperandSize::Unsized
        }
    }

    /// Checks whether the instruction has variable operand sizing.
    ///
    /// See [`Instruction::operand_size`] for details on what this means.
    pub fn is_sized(&self) -> bool {
        self.operand_size() != OperandSize::Unsized
    }

    /// Gets a vector of instruction [`Operand`]s.
    pub fn operands(&self) -> &Vec<Operand> {
        &self.operands
    }

    /*fn assemble_operand(&self, output: &mut Vec<u8>, arg: &Argument, operand: Operand) {
        // If necessary, evaluate the real value of the argument and re-call the method.
        if let Argument::SizeConverter(c) = arg {
            let real_arg = c(self.operand_size().value());
            return self.assemble_operand(output, &real_arg, operand);
        }

        // Resize the output buffer to fit the operand.
        codegen::resize_extend(output, arg.position() + arg.width());

        // Write the operand to the code buffer.
        match arg {
            Argument::UImm(imm) => imm.write_operand(output, operand),
            Argument::Imm(imm) => imm.write_operand(output, operand),

            Argument::Bitfield(imm) => imm.write_operand(output, operand),

            Argument::Register(reg) => reg.write_operand(output, operand),
            Argument::Flag(imm) => imm.write_operand(output, operand),

            Argument::Memory(mem) => mem.write_operand(output, operand),

            Argument::PcRel(imm) => imm.write_operand(output, operand.subtract_pc(self.pc)),

            Argument::SizeConverter(_) => unreachable!(),
        }
    }

    /// Assembles the instruction into its machine code representation and writes the
    /// code to `output`.
    pub fn assemble(self, output: &mut Vec<u8>) {
        if let Some(bytes) = self.raw_bytes {
            output.extend(bytes);
        } else {
            // Construct and write the instruction opcode.
            output.push(
                self.operand_size().value() << 6 | build_opcode_form(self.meta.a, self.meta.b),
            );

            // Write the instruction subopcode at its expected position.
            let subopcode_position = self.meta.subopcode_location.position() as usize;
            codegen::resize_extend(output, subopcode_position + 1);
            output[subopcode_position] = (output[subopcode_position]
                & !self.meta.subopcode_location.mask())
                | self.meta.subopcode_location.build_value(self.subopcode());

            // Write the instruction operands. We can safely assume they are valid.
            for (arg, operand) in self.meta.operands.iter().flatten().zip(self.operands()) {
                self.assemble_operand(output, arg, *operand);
            }
        }
    }*/
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.kind(), self.operand_size())?;
        for operand in self.operands() {
            write!(f, " {}", operand)?;
        }

        Ok(())
    }
}

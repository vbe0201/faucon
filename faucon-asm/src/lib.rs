use std::io::Read;

pub use instruction::*;
pub use opcode::*;
pub use operand::*;

pub mod disassembler;
pub mod instruction;
pub mod opcode;
pub mod operand;

/// The result of a Falcon ISA functions.
pub type Result<T, E = Error> = std::result::Result<T, E>;

/// Error types that are used by the crate.
///
/// These help in providing better diagnostics when using the crate
/// and to provide one uniform [`Result`] type within the crate.
///
/// [`Result`]: type.Result.html
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// Invalid instruction encountered.
    ///
    /// Provides the opcode of the instruction in question.
    InvalidInstruction(u8),
    /// An I/O error occurred while working with a reader.
    IoError,
    /// An EOF has been reached while streaming a binary's content through [`Read`].
    ///
    /// [`Read`]: https://doc.rust-lang.org/std/io/trait.Read.html
    Eof,
}

/// A Falcon Assembly instruction.
pub struct Instruction {
    /// The kind of instruction that is being wrapped.
    ///
    /// [`InstructionKind`]s generally identify instructions and
    /// their operands individually and provide helper methods for
    /// working with them.
    ///
    /// [`InstructionKind`]: ./instruction/enum.InstructionKind.html
    pub kind: InstructionKind,
    /// The raw bytes of an instruction.
    pub(crate) bytes: Vec<u8>,
}

impl Instruction {
    /// Constructs a new instruction from its kind and bytes representation.
    ///
    /// This function returns `None` if an invalid instruction was supplied.
    ///
    /// NOTE: Since users are not supposed to tamper with the internal
    /// representations of instructions, they should be obtained through a
    /// factory function, rather than creating own objects of them.
    pub(crate) fn new(kind: InstructionKind) -> Option<Self> {
        // Filter out invalid instructions.
        if kind.invalid() {
            return None;
        }

        Some(Instruction {
            kind,
            bytes: Vec::new(),
        })
    }

    /// Gets the opcode of the instruction.
    ///
    /// The opcode is the first byte of each instruction, from which a lot
    /// of useful disassembling details can be derived. Together with a
    /// subopcode, instructions can be identified uniquely.
    pub fn opcode(&self) -> u8 {
        // This is considered safe, since only valid instructions
        // can be used for constructing this type.
        self.kind.opcode().unwrap()
    }

    /// Gets the subopcode of an instruction.
    ///
    /// The subopcode can be placed in various locations and is necessary
    /// for identifying instructions uniquely in combination with the
    /// opcode.
    pub fn subopcode(&self) -> u8 {
        // This is considered safe, since only valid instructions
        // can be used for constructing this type.
        self.kind.subopcode().unwrap()
    }

    /// A vector of [`Operand`]s that belong to this instruction.
    ///
    /// See [`Instruction::operand_size`] to determine the size of
    /// operands individually per instruction.
    pub fn operands(&self) -> Option<Vec<Operand>> {
        // Since there are instructions that might not take any
        // operands at all, it is better to return the Option
        // instead of unwrapping.
        self.kind.operands()
    }

    /// Gets the size of instruction operands.
    ///
    /// The size is derived from the first byte of an instruction.
    pub fn operand_size(&self) -> OperandSize {
        OperandSize::from(self.opcode())
    }

    /// Feeds a slice of bytes to the internal representation of the instruction.
    ///
    /// This method is supposed to be called by the factory function that creates
    /// the instruction. For the reasoning behind this choice, see [`Instruction::new`].
    ///
    /// [`Instruction::new`]: struct.Instruction.html#method.new
    pub(crate) fn feed(&mut self, bytes: &[u8]) {
        self.bytes.extend(bytes);
    }
}

/// Disassembles the bytes from a given stream until an EOF is encountered.
///
/// This function tries to disassemble as many bytes as provided through the
/// [`Read`]er, returning a [`Vec`] of [`Instruction`]s on EOF. As a logical
/// consequence, any [`Error`] variant returned by this instruction is never
/// an EOF.
///
/// [`Read`]: https://doc.rust-lang.org/std/io/trait.Read.html
/// [`Vec`]: https://doc.rust-lang.org/std/vec/struct.Vec.html
/// [`Instruction`]: struct.Instruction.html
/// [`Error`]: enum.Error.html
pub fn disassemble<R: Read>(reader: &mut R) -> Result<Vec<Instruction>> {
    let mut instructions = Vec::new();

    loop {
        match disassembler::read_instruction(reader) {
            Ok(insn) => instructions.push(insn),
            Err(e) => {
                return if e == Error::Eof {
                    Ok(instructions)
                } else {
                    Err(e)
                }
            }
        }
    }
}

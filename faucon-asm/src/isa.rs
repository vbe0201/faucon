//! Falcon ISA definitions to be used by the assembler and the disassembler.

use std::fmt;

use faucon_asm_derive::Instruction;

use crate::arguments::*;
use crate::opcode::*;

// Helper macro that is used by faucon-asm-derive codegen.
macro_rules! instruction_meta {
    ($kind:ident, $op:tt, $subop:tt, $operands:expr) => {
        InstructionMeta::new(InstructionKind::$kind, $op as u8, $subop as u8, $operands)
    };
}

/// A collection of metadata for representing assembly instructions.
///
/// These helpers are stored in internal opcode lookup tables for identifying
/// and parsing instructions from their binary representation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InstructionMeta {
    /// The instruction kind that is represented by this object.
    pub kind: InstructionKind,
    /// The first part of an instruction's opcode, which can be obtained through
    /// [`get_opcode_form`].
    ///
    /// [`get_opcode_form`]: ../opcode/fn.get_opcode_form.html
    pub a: u8,
    /// The second part of an instruction's opcode, which can be obtained through
    /// [`get_opcode_form`].
    ///
    /// [`get_opcode_form`]: ../opcode/fn.get_opcode_form.html
    pub b: u8,
    /// The operand size on which the instruction operates.
    pub size: OperandSize,
    /// The subopcode of an instruction.
    ///
    /// If [`InstructionMeta::a`] is in the range of 0 through 2, the subopcode
    /// should be identical to [`InstructionMeta::b`].
    ///
    /// [`InstructionMeta::a`]: struct.InstructionMeta.html#structfield.a
    /// [`InstructionMeta::b`]: struct.InstructionMeta.html#structfield.b
    pub subopcode: u8,
    /// A vector of Arguments which work as a parser layer of packing or unpacking
    /// several instruction operands in the underlying raw bytes.
    pub operands: [Argument; 3],
}

impl InstructionMeta {
    /// Constructs a new [`InstructionMeta`] object from relevant instruction
    /// details.
    ///
    /// [`InstructionMeta`]: struct.InstructionMeta.html
    pub const fn new(
        kind: InstructionKind,
        opcode: u8,
        subopcode: u8,
        operands: [Argument; 3],
    ) -> Self {
        let (a, b) = get_opcode_form(opcode);
        let size = get_operand_size(opcode);

        InstructionMeta {
            kind,
            a,
            b,
            size,
            subopcode,
            operands,
        }
    }
}

/// Assembly instruction kinds within the Falcon ISA.
///
/// Through internal implementation details, this enum is responsible for
/// generating opcode lookup tables that can be used to identify instructions
/// and their variants.
#[derive(Clone, Debug, PartialEq, Eq, Instruction)]
pub enum InstructionKind {
    /// The EXIT instruction.
    ///
    /// Halts microcode execution and triggers the EXIT interrupt so that the
    /// processor can only be restarted by the host machine.
    #[insn(opcode = 0xF8, subopcode = 0x02, operands())]
    EXIT,

    /// The SLEEP instruction.
    ///
    /// Puts the processor into sleep state until an unmasked interrupt is
    /// received. Repeated until the given flag bit is cleared.
    #[insn(opcode = 0xF4, subopcode = 0x28, operands(FLAG))]
    SLEEP,

    /// The IRET instruction.
    ///
    /// Returns from an interrupt handler.
    #[insn(opcode = 0xF8, subopcode = 0x01, operands())]
    IRET,

    /// The TRAP instruction.
    ///
    /// Triggers a software trap.
    #[insn(opcode = 0xF8, subopcode = 0x08, operands(TRAP))]
    #[insn(opcode = 0xF8, subopcode = 0x09, operands(TRAP))]
    #[insn(opcode = 0xF8, subopcode = 0x0A, operands(TRAP))]
    #[insn(opcode = 0xF8, subopcode = 0x0B, operands(TRAP))]
    TRAP,

    /// An invalid or unknown instruction.
    XXX,
}

impl fmt::Display for InstructionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mnemonic = match self {
            _ => "???",
        };

        write!(f, "{}", mnemonic)
    }
}

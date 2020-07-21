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

        InstructionMeta {
            kind,
            a,
            b,
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, Instruction)]
pub enum InstructionKind {
    /// The CMPU instruction.
    ///
    /// Compares two unsigned values and sets ALU flags based on the result.
    #[insn(opcode = 0x30, subopcode = 0x04, operands(R2, I8ZXS))]
    #[insn(opcode = 0x31, subopcode = 0x04, operands(R2, I16ZXS))]
    #[insn(opcode = 0x24, subopcode = 0x04, operands(R2, R1))]
    CMPU,

    /// The CMPS instruction.
    ///
    /// Compares two signed values and sets ALU flags based on the result.
    #[insn(opcode = 0x30, subopcode = 0x05, operands(R2, I8SXS))]
    #[insn(opcode = 0x31, subopcode = 0x05, operands(R2, I16SXS))]
    #[insn(opcode = 0x25, subopcode = 0x05, operands(R2, R1))]
    CMPS,

    /// The CMP instruction.
    ///
    /// Compares two values and sets ALU flags based on the result.
    #[insn(opcode = 0x30, subopcode = 0x06, operands(R2, I8SXS))]
    #[insn(opcode = 0x31, subopcode = 0x06, operands(R2, I16SXS))]
    #[insn(opcode = 0x26, subopcode = 0x06, operands(R2, R1))]
    CMP,

    /// The CLEAR instruction.
    ///
    /// Clears the contents of a register.
    #[insn(opcode = 0x3D, subopcode = 0x04, operands(R2))]
    CLEAR,

    /// The ADD instruction.
    ///
    /// Computes the sum of two operands and stores the result.
    #[insn(opcode = 0xF5, subopcode = 0x30, operands(SP, I16SX32))]
    #[insn(opcode = 0xF9, subopcode = 0x01, operands(SP, R2))]
    ADD,

    /// The LD instruction.
    ///
    /// Loads a value from Falcon DMem to a register.
    #[insn(opcode = 0x18, subopcode = 0x08, operands(R1, MEMRI))]
    #[insn(opcode = 0x34, subopcode = 0x00, operands(R2, MEMSPI))]
    #[insn(opcode = 0x3A, subopcode = 0x00, operands(R2, MEMSPR))]
    #[insn(opcode = 0x3C, subopcode = 0x08, operands(R3, MEMRR))]
    LD,

    /// The PUSH instruction.
    ///
    /// Pushes a value onto the stack and increments the stack pointer by four.
    #[insn(opcode = 0xF9, subopcode = 0x00, operands(R2))]
    PUSH,

    /// THE POP instruction.
    ///
    /// Pops a value off the stack and increments the stack pointer by four.
    #[insn(opcode = 0xFC, subopcode = 0x00, operands(R2))]
    POP,

    /// The CALL instruction.
    ///
    /// Performs an unconditional branch to an absolute address, pushing
    /// the return address onto the stack.
    #[insn(opcode = 0xF3, subopcode = 0x03, operands(I16ZX32P1))]
    #[insn(opcode = 0xF4, subopcode = 0x21, operands(I8ZX32))]
    #[insn(opcode = 0xF9, subopcode = 0x05, operands(R2))]
    CALL,

    /// The LCALL instruction.
    ///
    /// Performs an unconditional branch to an absolute address, pushing
    /// the return address onto the stack.
    // FIXME: This is effectively just a CALL. Why is that a dedicated instruction?
    #[insn(opcode = 0x7E, subopcode = 0x01, operands(I24ZX32))]
    LCALL,

    /// The LJMP instruction.
    ///
    /// Performs an unconditional branch to an absolute address.
    // FIXME: This is effectively just a JMP. Why is that a dedicated instruction?
    #[insn(opcode = 0x3E, subopcode = 0x00, operands(I24ZX32))]
    LJMP,

    /// The RET instruction.
    ///
    /// Returns from a previous subroutine call.
    #[insn(opcode = 0xF8, subopcode = 0x00, operands())]
    RET,

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

    /// The PTLB instruction.
    ///
    /// Loads the TLB that covers a given physical page into a destination
    /// register.
    #[insn(opcode = 0xFE, subopcode = 0x02, operands(R1, R2))]
    PTLB,

    /// The VTLB instruction.
    ///
    /// Loads the TLB that covers a given virtual address into a destination
    /// register.
    #[insn(opcode = 0xFE, subopcode = 0x03, operands(R1, R2))]
    VTLB,

    /// The ITLB instruction.
    ///
    /// Clears a non-secret TLB entry corresponding to a specified physical
    /// page.
    #[insn(opcode = 0xF9, subopcode = 0x08, operands(R2))]
    ITLB,

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

    /// The XCLD instruction.
    ///
    /// Submits a DMA transfer request to load code from external memory.
    #[insn(opcode = 0xFA, subopcode = 0x04, operands(R2, R1))]
    XCLD,

    /// The XDLD instruction.
    ///
    /// Submits a DMA transfer request to load data from external memory.
    #[insn(opcode = 0xFA, subopcode = 0x05, operands(R2, R1))]
    XDLD,

    /// The XDST instruction.
    ///
    /// Submits a DMA transfer request to store local Falcon data in external
    /// memory.
    #[insn(opcode = 0xFA, subopcode = 0x06, operands(R2, R1))]
    XDST,

    /// The XCWAIT instruction.
    ///
    /// Waits for all DMA code load transfers to complete.
    #[insn(opcode = 0xF8, subopcode = 0x07, operands())]
    XCWAIT,

    /// The XDWAIT instruction.
    ///
    /// Waits for all DMA data load/store transfers to complete.
    #[insn(opcode = 0xF8, subopcode = 0x03, operands())]
    XDWAIT,

    /// An invalid or unknown instruction.
    XXX,
}

impl fmt::Display for InstructionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mnemonic = match self {
            InstructionKind::CMPU => "cmpu",
            InstructionKind::CMPS => "cmps",
            InstructionKind::CMP => "cmp",
            InstructionKind::CLEAR => "clear",
            InstructionKind::ADD => "add",
            InstructionKind::LD => "ld",
            InstructionKind::PUSH => "push",
            InstructionKind::POP => "pop",
            InstructionKind::CALL => "call",
            InstructionKind::LCALL => "lcall",
            InstructionKind::LJMP => "ljmp",
            InstructionKind::RET => "ret",
            InstructionKind::EXIT => "exit",
            InstructionKind::SLEEP => "sleep",
            InstructionKind::PTLB => "ptlb",
            InstructionKind::VTLB => "vtlb",
            InstructionKind::ITLB => "itlb",
            InstructionKind::IRET => "iret",
            InstructionKind::TRAP => "trap",
            InstructionKind::XCLD => "xcld",
            InstructionKind::XDLD => "xdld",
            InstructionKind::XDST => "xdst",
            InstructionKind::XCWAIT => "xcwait",
            InstructionKind::XDWAIT => "xdwait",
            InstructionKind::XXX => "???",
        };

        write!(f, "{}", mnemonic)
    }
}

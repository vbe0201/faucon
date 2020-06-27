//! Falcon Assembly instruction listings.

use faucon_asm_derive::Instruction;

use crate::operand::{get_opcode_meta, OperandMeta};

/// Assembly instructions that are supported by the Falcon ISA.
///
/// This enum is expanded by an internally used proc-macro which
/// simplifies parsing instructions, given an `(opcode, subopcode)`
/// pair. See the respective methods which ease up parsing work.
#[derive(Clone, Debug, PartialEq, Eq, Instruction)]
pub enum InstructionKind {
    /// The ADD instruction.
    ///
    /// Computes the sum of two operands and stores the
    /// result.
    #[insn(opcode = 0x10, subopcode = 0x00)]
    #[insn(opcode = 0x20, subopcode = 0x00)]
    #[insn(opcode = 0x36, subopcode = 0x00)]
    #[insn(opcode = 0x37, subopcode = 0x00)]
    #[insn(opcode = 0x3B, subopcode = 0x00)]
    #[insn(opcode = 0x3C, subopcode = 0x00)]
    ADD(u8, u8),

    /// The ADC instruction.
    ///
    /// Computes the sum of two operands with a carry and
    /// stores the result.
    #[insn(opcode = 0x10, subopcode = 0x01)]
    #[insn(opcode = 0x20, subopcode = 0x01)]
    #[insn(opcode = 0x36, subopcode = 0x01)]
    #[insn(opcode = 0x37, subopcode = 0x01)]
    #[insn(opcode = 0x3B, subopcode = 0x01)]
    #[insn(opcode = 0x3C, subopcode = 0x01)]
    ADC(u8, u8),

    /// The SUB instruction.
    ///
    /// Computes the difference of two operands and stores the
    /// result.
    #[insn(opcode = 0x10, subopcode = 0x02)]
    #[insn(opcode = 0x20, subopcode = 0x02)]
    #[insn(opcode = 0x36, subopcode = 0x02)]
    #[insn(opcode = 0x37, subopcode = 0x02)]
    #[insn(opcode = 0x3B, subopcode = 0x02)]
    #[insn(opcode = 0x3C, subopcode = 0x02)]
    SUB(u8, u8),

    /// The SBB instruction.
    ///
    /// Computes the difference of two operands with a borrow and
    /// stores the result.
    #[insn(opcode = 0x10, subopcode = 0x03)]
    #[insn(opcode = 0x20, subopcode = 0x03)]
    #[insn(opcode = 0x36, subopcode = 0x03)]
    #[insn(opcode = 0x37, subopcode = 0x03)]
    #[insn(opcode = 0x3B, subopcode = 0x03)]
    #[insn(opcode = 0x3C, subopcode = 0x03)]
    SBB(u8, u8),

    /// The AND instruction.
    ///
    /// Applies a bitwise and operation on two operands and stores
    /// the result.
    #[insn(opcode = 0xC0, subopcode = 0x04)]
    #[insn(opcode = 0xE0, subopcode = 0x04)]
    #[insn(opcode = 0xF0, subopcode = 0x04)]
    #[insn(opcode = 0xF1, subopcode = 0x04)]
    #[insn(opcode = 0xFD, subopcode = 0x04)]
    #[insn(opcode = 0xFF, subopcode = 0x04)]
    AND(u8, u8),

    /// The OR instruction.
    ///
    /// Applies a bitwise or operation on two operands and stores
    /// the result.
    #[insn(opcode = 0xC0, subopcode = 0x05)]
    #[insn(opcode = 0xE0, subopcode = 0x05)]
    #[insn(opcode = 0xF0, subopcode = 0x05)]
    #[insn(opcode = 0xF1, subopcode = 0x05)]
    #[insn(opcode = 0xFD, subopcode = 0x05)]
    #[insn(opcode = 0xFF, subopcode = 0x05)]
    OR(u8, u8),

    /// The XOR instruction.
    ///
    /// Applies a bitwise xor operation on two operands and stores
    /// the result.
    #[insn(opcode = 0xC0, subopcode = 0x06)]
    #[insn(opcode = 0xE0, subopcode = 0x06)]
    #[insn(opcode = 0xF0, subopcode = 0x06)]
    #[insn(opcode = 0xF1, subopcode = 0x06)]
    #[insn(opcode = 0xFD, subopcode = 0x06)]
    #[insn(opcode = 0xFF, subopcode = 0x06)]
    XOR(u8, u8),

    /// The XBIT instruction.
    ///
    /// Extracts a single bit of a specified register and stores it in the
    /// highest bit of the destination register, setting all other bits to 0.
    #[insn(opcode = 0xC0, subopcode = 0x08)]
    #[insn(opcode = 0xFF, subopcode = 0x08)]
    #[insn(opcode = 0xF0, subopcode = 0x0C)]
    #[insn(opcode = 0xFE, subopcode = 0x0C)]
    XBIT(u8, u8),

    /// The BSET instruction.
    ///
    /// Sets a specific bit in a given register.
    #[insn(opcode = 0xF0, subopcode = 0x09)]
    #[insn(opcode = 0xFD, subopcode = 0x09)]
    #[insn(opcode = 0xF4, subopcode = 0x31)]
    #[insn(opcode = 0xF9, subopcode = 0x09)]
    BSET(u8, u8),

    /// The BCLR instruction.
    ///
    /// Clears a specific bit in a given register.
    #[insn(opcode = 0xF0, subopcode = 0x0A)]
    #[insn(opcode = 0xFD, subopcode = 0x0A)]
    #[insn(opcode = 0xF4, subopcode = 0x32)]
    #[insn(opcode = 0xF9, subopcode = 0x0A)]
    BCLR(u8, u8),

    /// The BTGL instruction.
    ///
    /// Toggles (flips) a specific bit in a given register.
    #[insn(opcode = 0xF0, subopcode = 0x0B)]
    #[insn(opcode = 0xFD, subopcode = 0x0B)]
    #[insn(opcode = 0xF4, subopcode = 0x33)]
    #[insn(opcode = 0xF9, subopcode = 0x0B)]
    BTGL(u8, u8),

    /// The SETP instruction.
    ///
    /// Sets a specific bit in the flags register to the highest
    /// bit of the first operand.
    #[insn(opcode = 0xF2, subopcode = 0x8)]
    #[insn(opcode = 0xFA, subopcode = 0x8)]
    SETP(u8, u8),

    /// The IOWR instruction.
    ///
    /// Writes a word to the I/O space of the processor.
    #[insn(opcode = 0xFA, subopcode = 0x0)]
    IOWR(u8, u8),

    /// An invalid or unknown instruction.
    ///
    /// This can be checked through calling [`InstructionKind::invalid`].
    /// If such an instruction is encountered, the user may halt the
    /// program.
    ///
    /// [`InstructionKind::invalid`]: enum.InstructionKind.html#method.invalid
    XXX,
}

use faucon_asm_derive::Instruction;

use crate::operand::Operand;

/// Assembly instructions that are supported by the Falcon ISA.
///
/// This enum is expanded by an internally used proc-macro which
/// simplifies parsing instructions, given an `(opcode, subopcode)`
/// pair. See the respective methods which ease up parsing work.
#[derive(Clone, Debug, PartialEq, Eq, Instruction)]
pub enum InstructionKind {
    /// The XOR instruction.
    ///
    /// Applies a bitwise xor operation on two operands and stores
    /// the result.
    #[insn(opcode = 0xC0, subopcode = 6, operands = "R1D, R2S, I8")]
    XOR(u8, u8, String),

    /// An invalid or unknown instruction.
    ///
    /// This can be checked through calling [`InstructionKind::invalid`].
    /// If such an instruction is encountered, the user may halt the
    /// program.
    ///
    /// [`InstructionKind::invalid`]: struct.InstructionKind.html#method.invalid
    XXX,
}

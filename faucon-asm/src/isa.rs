//! Falcon ISA definitions to be used by the assembler and the disassembler.

use std::fmt;

use faucon_asm_derive::Instruction;

use crate::arguments::*;
use crate::opcode::*;

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
    pub operands: [Option<Argument>; 3],
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
        operands: [Option<Argument>; 3],
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

    /// The ADD instruction.
    ///
    /// Computes the sum of two operands and stores the result.
    #[insn(opcode = 0x10, subopcode = 0x00, operands(R1, R2, I8ZXS))]
    #[insn(opcode = 0x36, subopcode = 0x00, operands(R2, R2, I8ZXS))]
    #[insn(opcode = 0x37, subopcode = 0x00, operands(R2, R2, I16ZXS))]
    #[insn(opcode = 0x38, subopcode = 0x00, operands(R1, R2, I16ZXS))]
    #[insn(opcode = 0x3B, subopcode = 0x00, operands(R2, R2, R1))]
    #[insn(opcode = 0x3C, subopcode = 0x00, operands(R3, R2, R1))]
    #[insn(opcode = 0xF4, subopcode = 0x30, operands(SP, SP, I8SX32))]
    #[insn(opcode = 0xF5, subopcode = 0x30, operands(SP, SP, I16SX32))]
    #[insn(opcode = 0xF9, subopcode = 0x01, operands(SP, SP, R2))]
    ADD,

    /// The ADC instruction.
    ///
    /// Computes the sum of two operands with a carry and stores the result.
    #[insn(opcode = 0x11, subopcode = 0x01, operands(R1, R2, I8ZXS))]
    #[insn(opcode = 0x36, subopcode = 0x01, operands(R2, R2, I8ZXS))]
    #[insn(opcode = 0x37, subopcode = 0x01, operands(R2, R2, I16ZXS))]
    #[insn(opcode = 0x38, subopcode = 0x01, operands(R1, R2, I16ZXS))]
    #[insn(opcode = 0x3B, subopcode = 0x01, operands(R2, R2, R1))]
    #[insn(opcode = 0x3C, subopcode = 0x01, operands(R3, R2, R1))]
    ADC,

    /// The SUB instruction.
    ///
    /// Subtracts two operands and stores the result.
    #[insn(opcode = 0x12, subopcode = 0x02, operands(R1, R2, I8ZXS))]
    #[insn(opcode = 0x36, subopcode = 0x02, operands(R2, R2, I8ZXS))]
    #[insn(opcode = 0x37, subopcode = 0x02, operands(R2, R2, I16ZXS))]
    #[insn(opcode = 0x38, subopcode = 0x02, operands(R1, R2, I16ZXS))]
    #[insn(opcode = 0x3B, subopcode = 0x02, operands(R2, R2, R1))]
    #[insn(opcode = 0x3C, subopcode = 0x02, operands(R3, R2, R1))]
    SUB,

    /// The SBB instruction.
    ///
    /// Subtracts two operands with borrow and stores the result.
    #[insn(opcode = 0x13, subopcode = 0x03, operands(R1, R2, I8ZXS))]
    #[insn(opcode = 0x36, subopcode = 0x03, operands(R2, R2, I8ZXS))]
    #[insn(opcode = 0x37, subopcode = 0x03, operands(R2, R2, I16ZXS))]
    #[insn(opcode = 0x38, subopcode = 0x03, operands(R1, R2, I16ZXS))]
    #[insn(opcode = 0x3B, subopcode = 0x03, operands(R2, R2, R1))]
    #[insn(opcode = 0x3C, subopcode = 0x03, operands(R3, R2, R1))]
    SBB,

    /// The SHL instruction.
    ///
    /// Shifts a value left and stores the result.
    #[insn(opcode = 0x14, subopcode = 0x04, operands(R1, R2, I8ZXS))]
    #[insn(opcode = 0x36, subopcode = 0x04, operands(R2, R2, I8ZXS))]
    #[insn(opcode = 0x3B, subopcode = 0x04, operands(R2, R2, R1))]
    #[insn(opcode = 0x3C, subopcode = 0x04, operands(R3, R2, R1))]
    SHL,

    /// The SHR instruction.
    ///
    /// Shifts a value right and stores the result.
    #[insn(opcode = 0x15, subopcode = 0x05, operands(R1, R2, I8ZXS))]
    #[insn(opcode = 0x36, subopcode = 0x05, operands(R2, R2, I8ZXS))]
    #[insn(opcode = 0x3B, subopcode = 0x05, operands(R2, R2, R1))]
    #[insn(opcode = 0x3C, subopcode = 0x05, operands(R3, R2, R1))]
    SHR,

    /// The SAR instruction.
    ///
    /// Shifts a value right with sign bit and stores the result.
    #[insn(opcode = 0x17, subopcode = 0x07, operands(R1, R2, I8ZXS))]
    #[insn(opcode = 0x36, subopcode = 0x07, operands(R2, R2, I8ZXS))]
    #[insn(opcode = 0x3B, subopcode = 0x07, operands(R2, R2, R1))]
    #[insn(opcode = 0x3C, subopcode = 0x07, operands(R3, R2, R1))]
    SAR,

    /// The SHLC instruction.
    ///
    /// Shifts a value left with carry in and stores the result.
    #[insn(opcode = 0x1C, subopcode = 0x0C, operands(R1, R2, I8ZXS))]
    #[insn(opcode = 0x36, subopcode = 0x0C, operands(R2, R2, I8ZXS))]
    #[insn(opcode = 0x3B, subopcode = 0x0C, operands(R2, R2, R1))]
    #[insn(opcode = 0x3C, subopcode = 0x0C, operands(R3, R2, R1))]
    SHLC,

    /// The SHRC instruction.
    ///
    /// Shifts a value right with carry in and stores the result.
    #[insn(opcode = 0x1D, subopcode = 0x0D, operands(R1, R2, I8ZXS))]
    #[insn(opcode = 0x36, subopcode = 0x0D, operands(R2, R2, I8ZXS))]
    #[insn(opcode = 0x3B, subopcode = 0x0D, operands(R2, R2, R1))]
    #[insn(opcode = 0x3C, subopcode = 0x0D, operands(R3, R2, R1))]
    SHRC,

    /// The NOT instruction.
    ///
    /// Flips all bits in a value.
    #[insn(opcode = 0x39, subopcode = 0x00, operands(R1, R2))]
    #[insn(opcode = 0x3D, subopcode = 0x01, operands(R2, R2))]
    NOT,

    /// The NEG instruction.
    ///
    /// Negates a value
    #[insn(opcode = 0x39, subopcode = 0x01, operands(R1, R2))]
    #[insn(opcode = 0x3D, subopcode = 0x00, operands(R2, R2))]
    NEG,

    /// The HSWAP instruction.
    ///
    ///  Rotates a value by half it's size
    #[insn(opcode = 0x39, subopcode = 0x03, operands(R1, R2))]
    #[insn(opcode = 0x3D, subopcode = 0x03, operands(R2, R2))]
    HSWAP,

    /// The SETHI instruction.
    ///
    /// Sets the high 16 bits of a register to a value, without thouching
    /// the low 16 bits.
    #[insn(opcode = 0xF0, subopcode = 0x03, operands(R2, I8ZX32S16))]
    SETHI,

    /// The CLEAR instruction.
    ///
    /// Clears the contents of a register.
    #[insn(opcode = 0x3D, subopcode = 0x04, operands(R2))]
    CLEAR,

    /// THE MULU instruction.
    ///
    /// Performs an unsigned multiplication and stores the result.
    #[insn(opcode = 0xC0, subopcode = 0x00, operands(R1, R2, I8ZX32))]
    #[insn(opcode = 0xE0, subopcode = 0x00, operands(R1, R2, I16ZX32))]
    #[insn(opcode = 0xF0, subopcode = 0x00, operands(R2, R2, I8ZX32))]
    #[insn(opcode = 0xFD, subopcode = 0x00, operands(R2, R2, R1))]
    #[insn(opcode = 0xFF, subopcode = 0x00, operands(R3, R2, R1))]
    MULU,

    /// The MULS instruction.
    ///
    /// Performs a signed multiplication and stores the result.
    #[insn(opcode = 0xC1, subopcode = 0x01, operands(R1, R2, I8SX32))]
    #[insn(opcode = 0xE1, subopcode = 0x01, operands(R1, R2, I16SX32))]
    #[insn(opcode = 0xF0, subopcode = 0x01, operands(R2, R2, I8SX32))]
    #[insn(opcode = 0xFD, subopcode = 0x01, operands(R2, R2, R1))]
    #[insn(opcode = 0xFF, subopcode = 0x01, operands(R3, R2, R1))]
    MULS,

    /// The SEXT instruction.
    ///
    /// Sign-extends a value and stores the result.
    #[insn(opcode = 0xC2, subopcode = 0x02, operands(R1, R2, I8))]
    #[insn(opcode = 0xF0, subopcode = 0x02, operands(R2, R2, I8))]
    #[insn(opcode = 0xFD, subopcode = 0x02, operands(R2, R2, R1))]
    #[insn(opcode = 0xFF, subopcode = 0x02, operands(R3, R2, R1))]
    SEXT,

    /// The AND instruction.
    ///
    /// Performs a binary AND operation on two operands.
    #[insn(opcode = 0xC4, subopcode = 0x04, operands(R1, R2, I8ZX32))]
    #[insn(opcode = 0xE4, subopcode = 0x04, operands(R1, R2, I16ZX32))]
    #[insn(opcode = 0xF0, subopcode = 0x04, operands(R2, R2, I8ZX32))]
    #[insn(opcode = 0xF1, subopcode = 0x04, operands(R2, R2, I16ZX32))]
    #[insn(opcode = 0xFD, subopcode = 0x04, operands(R2, R2, R1))]
    #[insn(opcode = 0xFF, subopcode = 0x04, operands(R3, R2, R1))]
    AND,

    /// The OR instruction.
    ///
    /// Performs a binary OR operation on two operands.
    #[insn(opcode = 0xC5, subopcode = 0x05, operands(R1, R2, I8ZX32))]
    #[insn(opcode = 0xE5, subopcode = 0x05, operands(R1, R2, I16ZX32))]
    #[insn(opcode = 0xF0, subopcode = 0x05, operands(R2, R2, I8ZX32))]
    #[insn(opcode = 0xF1, subopcode = 0x05, operands(R2, R2, I16ZX32))]
    #[insn(opcode = 0xFD, subopcode = 0x05, operands(R2, R2, R1))]
    #[insn(opcode = 0xFF, subopcode = 0x05, operands(R3, R2, R1))]
    OR,

    /// The XOR instruction.
    ///
    /// Performs a binary XOR operation on two operands.
    #[insn(opcode = 0xC6, subopcode = 0x06, operands(R1, R2, I8ZX32))]
    #[insn(opcode = 0xE6, subopcode = 0x06, operands(R1, R2, I16ZX32))]
    #[insn(opcode = 0xF0, subopcode = 0x06, operands(R2, R2, I8ZX32))]
    #[insn(opcode = 0xF1, subopcode = 0x06, operands(R2, R2, I16ZX32))]
    #[insn(opcode = 0xFD, subopcode = 0x06, operands(R2, R2, R1))]
    #[insn(opcode = 0xFF, subopcode = 0x06, operands(R3, R2, R1))]
    XOR,

    /// The XBIT instruction.
    ///
    /// Extracts a bit from a specified register and stores it in the lowest
    /// bit of the destination register, setting all other bits to 0.
    #[insn(opcode = 0xC8, subopcode = 0x08, operands(R1, R2, I8))]
    #[insn(opcode = 0xFF, subopcode = 0x08, operands(R3, R2, R1))]
    #[insn(opcode = 0xF0, subopcode = 0x0C, operands(R2, FLAGS, FLAG))]
    #[insn(opcode = 0xFE, subopcode = 0x0C, operands(R1, FLAGS, R2))]
    XBIT,

    /// The BSET instruction.
    ///
    /// Sets a specific bit in a given register.
    #[insn(opcode = 0xF0, subopcode = 0x09, operands(R2, I8))]
    #[insn(opcode = 0xFD, subopcode = 0x09, operands(R2, R1))]
    #[insn(opcode = 0xF4, subopcode = 0x31, operands(FLAGS, FLAG))]
    #[insn(opcode = 0xF9, subopcode = 0x09, operands(FLAGS, R2))]
    BSET,

    /// The BCLR instruction.
    ///
    /// Clears a specific bit in a given register.
    #[insn(opcode = 0xF0, subopcode = 0x0A, operands(R2, I8))]
    #[insn(opcode = 0xFD, subopcode = 0x0A, operands(R2, R1))]
    #[insn(opcode = 0xF4, subopcode = 0x32, operands(FLAGS, FLAG))]
    #[insn(opcode = 0xF9, subopcode = 0x0A, operands(FLAGS, R2))]
    BCLR,

    /// The BTGL instruction.
    ///
    /// Toggles (flips) a specific bit in a given register.
    #[insn(opcode = 0xF0, subopcode = 0x0B, operands(R2, I8))]
    #[insn(opcode = 0xFD, subopcode = 0x0B, operands(R2, R1))]
    #[insn(opcode = 0xF4, subopcode = 0x33, operands(FLAGS, FLAG))]
    #[insn(opcode = 0xF9, subopcode = 0x0B, operands(FLAGS, R2))]
    BTGL,

    /// The DIV instruction.
    ///
    /// Performs unsigned 32-bit division on two operands.
    #[insn(opcode = 0xCC, subopcode = 0x0C, operands(R1, R2, I8ZX32))]
    #[insn(opcode = 0xEC, subopcode = 0x0C, operands(R1, R2, I16ZX32))]
    #[insn(opcode = 0xFF, subopcode = 0x0C, operands(R3, R2, R1))]
    DIV,

    /// The MOD instruction.
    ///
    /// Takes the modulus of two 32-bit unsigned operands.
    #[insn(opcode = 0xCD, subopcode = 0x0D, operands(R1, R2, I8ZX32))]
    #[insn(opcode = 0xED, subopcode = 0x0D, operands(R1, R2, I16ZX32))]
    #[insn(opcode = 0xFF, subopcode = 0x0D, operands(R3, R2, R1))]
    MOD,

    /// The SETP instruction.
    ///
    /// Sets a given bit in the `$flags` register to the lowest bit of the
    /// source register.
    #[insn(opcode = 0xF2, subopcode = 0x08, operands(FLAG, R2))]
    #[insn(opcode = 0xFA, subopcode = 0x08, operands(R1, R2))]
    SETP,

    /// The MOV instruction.
    ///
    /// Moves values of immediates or registers to other registers.
    #[insn(opcode = 0x00, subopcode = 0x00, operands(R0, I8SX32P1))]
    #[insn(opcode = 0x40, subopcode = 0x01, operands(R0, I16SX32P1))]
    #[insn(opcode = 0x80, subopcode = 0x02, operands(R0, I24SX32))]
    #[insn(opcode = 0x32, subopcode = 0x02, operands(R1, R2))]
    #[insn(opcode = 0xD0, subopcode = 0x00, operands(R0, I32))]
    #[insn(opcode = 0xFE, subopcode = 0x00, operands(SR2, R2))]
    #[insn(opcode = 0xFE, subopcode = 0x01, operands(R1, SR1))]
    MOV,

    /// The LD instruction.
    ///
    /// Loads a value from Falcon DMem to a register.
    #[insn(opcode = 0x18, subopcode = 0x08, operands(R1, MEMRI))]
    #[insn(opcode = 0x34, subopcode = 0x00, operands(R2, MEMSPI))]
    #[insn(opcode = 0x3A, subopcode = 0x00, operands(R2, MEMSPR))]
    #[insn(opcode = 0x3C, subopcode = 0x08, operands(R3, MEMRR))]
    #[insn(opcode = 0x3F, subopcode = 0x0F, operands(R1, MEMR))]
    LD,

    /// The ST instruction.
    ///
    /// Stores a value from a register to Falcon DMem.
    #[insn(opcode = 0x20, subopcode = 0x00, operands(MEMR, R1))]
    #[insn(opcode = 0x21, subopcode = 0x01, operands(MEMSPR, R2))]
    #[insn(opcode = 0x30, subopcode = 0x01, operands(MEMSPI, R2))]
    #[insn(opcode = 0x35, subopcode = 0x05, operands(MEMRI, R1))]
    #[insn(opcode = 0x3C, subopcode = 0x09, operands(MEMRRALT, R1))]
    ST,

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

    /// The MPUSH instruction.
    ///
    /// Pushes all registers in the range from $r0 to $rX (the supplied operand)
    /// onto the stack.
    #[insn(opcode = 0xF9, subopcode = 0x02, operands(R2))]
    MPUSH,

    /// The MPOP instruction.
    ///
    /// Pops as many values off the stack as there are registers in the range from
    /// $r0 to $rX (the supplied operand).
    #[insn(opcode = 0xFB, subopcode = 0x00, operands(R2))]
    MPOP,

    /// The MPOPADD instruction.
    ///
    /// This instruction essentially executes a [`InstructionKind::MPOP`] and finally
    /// adds the supplied immediate value to the $sp register.
    ///
    /// [`InstructionKind::MPOP`]: enum.InstructionKind.html#variant.MPOP
    #[insn(opcode = 0xFB, subopcode = 0x02, operands(R2, I16SX32))]
    #[insn(opcode = 0xFB, subopcode = 0x04, operands(R2, I8SX32))]
    MPOPADD,

    /// The MPOPRET instruction.
    ///
    /// This instruction essentially executes a [`InstructionKind::MPOP`] followed by
    /// a [`InstructionKind::RET`].
    ///
    /// [`InstructionKind::MPOP`]: enum.InstructionKind.html#variant.MPOP
    /// [`InstructionKind::RET`]: enum.InstructionKind.html#variant.RET
    #[insn(opcode = 0xFB, subopcode = 0x01, operands(R2))]
    MPOPRET,

    /// The MPOPADDRET instruction.
    ///
    /// This instruction essentially executes a [`InstructionKind::MPOPADD`] followed
    /// by a [`InstructionKind::RET`].
    ///
    /// [`InstructionKind::MPOPADD`]: enum.InstructionKind.html#variant.MPOPADD
    /// [`InstructionKind::RET`]: enum.InstructionKind.html#variant.RET
    #[insn(opcode = 0xFB, subopcode = 0x03, operands(R2, I16SX32))]
    #[insn(opcode = 0xFB, subopcode = 0x05, operands(R2, I8SX32))]
    MPOPADDRET,

    /// The CALL instruction.
    ///
    /// Performs an unconditional call to an absolute address, pushing
    /// the return address onto the stack.
    #[insn(opcode = 0xF3, subopcode = 0x03, operands(I16ZX32P1))]
    #[insn(opcode = 0xF4, subopcode = 0x21, operands(I8ZX32))]
    #[insn(opcode = 0xF9, subopcode = 0x05, operands(R2))]
    CALL,

    /// The LCALL instruction.
    ///
    /// Performs an unconditional long call to an absolute address,
    /// pushing the return address onto the stack.
    #[insn(opcode = 0x7E, subopcode = 0x01, operands(I24ZX32))]
    LCALL,

    /// The BRA instruction.
    ///
    /// Performs an unconditional branch to an absolute or PC-relative
    /// address.
    #[insn(opcode = 0xF4, subopcode = 0x20, operands(I8ZX32))]
    #[insn(opcode = 0xF5, subopcode = 0x20, operands(I16ZX32))]
    #[insn(opcode = 0xF9, subopcode = 0x04, operands(R2))]
    BRA,

    /// The BC instruction.
    ///
    /// Branches to the PC-relative target when the carry bit is set.
    #[insn(opcode = 0xF4, subopcode = 0x08, operands(PC8))]
    #[insn(opcode = 0xF5, subopcode = 0x08, operands(PC16))]
    BC,

    /// The LBRA instruction.
    ///
    /// Performs an unconditional long branch to an absolute address.
    #[insn(opcode = 0x3E, subopcode = 0x00, operands(I24ZX32))]
    LBRA,

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

    /// The XDFENCE instruction.
    ///
    /// Constructs a memory barrier for DMA data transfers, ensuring that
    /// all transfers queried prior to constructing the barrier will be
    /// finished before the ones after it.
    #[insn(opcode = 0xF8, subopcode = 0x06, operands())]
    XDFENCE,

    /// The IOWR instruction.
    ///
    /// Asynchronously writes a word to the I/O space of the microprocessor.
    #[insn(opcode = 0xF6, subopcode = 0x06, operands(IORI, R1))]
    #[insn(opcode = 0xFA, subopcode = 0x00, operands(IOR, R1))]
    IOWR,

    /// The IOWRS instruction.
    ///
    /// Synchronously writes a word to the I/O space of the microprocessor.
    #[insn(opcode = 0xF7, subopcode = 0x07, operands(IORI, R1))]
    #[insn(opcode = 0xFA, subopcode = 0x01, operands(IOR, R2))]
    IOWRS,

    /// The IORD instruction.
    ///
    /// Asynchronously reads a word from the I/O space of the microprocessor.
    #[insn(opcode = 0xCF, subopcode = 0x0F, operands(R1, IORI))]
    #[insn(opcode = 0xFF, subopcode = 0x0F, operands(R3, IORR))]
    IORD,

    /// The IORDS instruction.
    ///
    /// Synchronously reads a word from the I/O space of the microprocessor.
    #[insn(opcode = 0xFF, subopcode = 0x0E, operands(R3, IORR))]
    IORDS,
}

impl fmt::Display for InstructionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mnemonic = match self {
            InstructionKind::CMPU => "cmpu",
            InstructionKind::CMPS => "cmps",
            InstructionKind::CMP => "cmp",
            InstructionKind::ADD => "add",
            InstructionKind::ADC => "adc",
            InstructionKind::SUB => "sub",
            InstructionKind::SBB => "sbb",
            InstructionKind::SHL => "shl",
            InstructionKind::SHR => "shr",
            InstructionKind::SAR => "sar",
            InstructionKind::SHLC => "shlc",
            InstructionKind::SHRC => "shrc",
            InstructionKind::NOT => "not",
            InstructionKind::NEG => "neg",
            InstructionKind::HSWAP => "hswap",
            InstructionKind::SETHI => "sethi",
            InstructionKind::CLEAR => "clear",
            InstructionKind::MULU => "mulu",
            InstructionKind::MULS => "muls",
            InstructionKind::SEXT => "sext",
            InstructionKind::AND => "and",
            InstructionKind::OR => "or",
            InstructionKind::XOR => "xor",
            InstructionKind::XBIT => "xbit",
            InstructionKind::BSET => "bset",
            InstructionKind::BCLR => "bclr",
            InstructionKind::BTGL => "btgl",
            InstructionKind::DIV => "div",
            InstructionKind::MOD => "mod",
            InstructionKind::SETP => "setp",
            InstructionKind::MOV => "mov",
            InstructionKind::LD => "ld",
            InstructionKind::ST => "st",
            InstructionKind::PUSH => "push",
            InstructionKind::POP => "pop",
            InstructionKind::MPUSH => "mpush",
            InstructionKind::MPOP => "mpop",
            InstructionKind::MPOPADD => "mpopadd",
            InstructionKind::MPOPRET => "mpopret",
            InstructionKind::MPOPADDRET => "mpopaddret",
            InstructionKind::CALL => "call",
            InstructionKind::LCALL => "lcall",
            InstructionKind::BRA => "bra",
            InstructionKind::BC => "bc",
            InstructionKind::LBRA => "lbra",
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
            InstructionKind::XDFENCE => "xdfence",
            InstructionKind::IOWR => "iowr",
            InstructionKind::IOWRS => "iowrs",
            InstructionKind::IORD => "iord",
            InstructionKind::IORDS => "iords",
        };

        write!(f, "{}", mnemonic)
    }
}

//! Falcon Assembly instruction listings.

use std::fmt;

use faucon_asm_derive::Instruction;

use crate::operand::{get_opcode_meta, OperandMeta};

/// Assembly instructions that are supported by the Falcon ISA.
///
/// This enum is expanded by an internally used proc-macro which
/// simplifies parsing instructions, given an `(opcode, subopcode)`
/// pair. See the respective methods which ease up parsing work.
#[derive(Clone, Debug, PartialEq, Eq, Instruction)]
pub enum InstructionKind {
    /// The CMPU instruction.
    ///
    /// Compares two unsigned values and sets ALU flags based on the
    /// result.
    #[insn(opcode = 0x30, subopcode = 0x04)]
    #[insn(opcode = 0x31, subopcode = 0x04)]
    #[insn(opcode = 0x38, subopcode = 0x04)]
    CMPU(u8, u8),

    /// The CMPS instruction.
    ///
    /// Compares two signed values and sets ALU flags based on the
    /// result.
    #[insn(opcode = 0x30, subopcode = 0x05)]
    #[insn(opcode = 0x31, subopcode = 0x05)]
    #[insn(opcode = 0x38, subopcode = 0x05)]
    CMPS(u8, u8),

    /// The CMP instruction.
    ///
    /// Compares two values and sets ALU flags based on the result.
    #[insn(opcode = 0x30, subopcode = 0x06)]
    #[insn(opcode = 0x31, subopcode = 0x06)]
    #[insn(opcode = 0x38, subopcode = 0x06)]
    CMP(u8, u8),

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

    /// The SHL instruction.
    ///
    /// Shifts the bits of a value to left and stores the result.
    #[insn(opcode = 0x10, subopcode = 0x04)]
    #[insn(opcode = 0x36, subopcode = 0x04)]
    #[insn(opcode = 0x3B, subopcode = 0x04)]
    #[insn(opcode = 0x3C, subopcode = 0x04)]
    SHL(u8, u8),

    /// The SHR instruction.
    ///
    /// Shifts the bits of a value to right and stores the result.
    #[insn(opcode = 0x10, subopcode = 0x05)]
    #[insn(opcode = 0x36, subopcode = 0x05)]
    #[insn(opcode = 0x3B, subopcode = 0x05)]
    #[insn(opcode = 0x3C, subopcode = 0x05)]
    SHR(u8, u8),

    /// The SAR instruction.
    ///
    /// Shifts the bits of a value to right with the sign bit and
    /// stores the result.
    #[insn(opcode = 0x10, subopcode = 0x06)]
    #[insn(opcode = 0x36, subopcode = 0x06)]
    #[insn(opcode = 0x3B, subopcode = 0x06)]
    #[insn(opcode = 0x3C, subopcode = 0x06)]
    SAR(u8, u8),

    /// The SHLC instruction.
    ///
    /// Shifts the bits of a value to left with a carry in and stores
    /// the result.
    #[insn(opcode = 0x10, subopcode = 0x0C)]
    #[insn(opcode = 0x36, subopcode = 0x0C)]
    #[insn(opcode = 0x3B, subopcode = 0x0C)]
    #[insn(opcode = 0x3C, subopcode = 0x0C)]
    SHLC(u8, u8),

    /// The SHRC instruction.
    ///
    /// Shifts the bits of a value to right with a carry in and stores
    /// the result.
    #[insn(opcode = 0x10, subopcode = 0x0D)]
    #[insn(opcode = 0x36, subopcode = 0x0D)]
    #[insn(opcode = 0x3B, subopcode = 0x0D)]
    #[insn(opcode = 0x3C, subopcode = 0x0D)]
    SHRC(u8, u8),

    /// The NOT instruction.
    ///
    /// Flips all bits in a value by performing a bitwise
    /// complement.
    #[insn(opcode = 0x39, subopcode = 0x00)]
    #[insn(opcode = 0x3D, subopcode = 0x00)]
    NOT(u8, u8),

    /// The NEG instruction.
    ///
    /// Negates a value.
    #[insn(opcode = 0x39, subopcode = 0x01)]
    #[insn(opcode = 0x3D, subopcode = 0x01)]
    NEG(u8, u8),

    /// The MOV instruction.
    ///
    /// Moves a value from one register to another.
    #[insn(opcode = 0x39, subopcode = 0x02)]
    #[insn(opcode = 0x3D, subopcode = 0x02)]
    MOV(u8, u8),

    /// The HSWAP instruction.
    ///
    /// Rotates a value by half its size and swaps these halves.
    #[insn(opcode = 0x39, subopcode = 0x03)]
    #[insn(opcode = 0x3D, subopcode = 0x03)]
    HSWAP(u8, u8),

    /// The LIM instruction.
    ///
    /// Loads an immediate into a register.
    #[insn(opcode = 0xF0, subopcode = 0x07)]
    #[insn(opcode = 0xF1, subopcode = 0x07)]
    LIM(u8, u8),

    /// The SETHI instruction.
    ///
    /// Sets the high 16 bits of a register to an immediate.
    #[insn(opcode = 0xF0, subopcode = 0x03)]
    #[insn(opcode = 0xF1, subopcode = 0x03)]
    SETHI(u8, u8),

    /// The CLR instruction.
    ///
    /// Clears a register.
    #[insn(opcode = 0x3D, subopcode = 0x04)]
    CLR(u8, u8),

    /// The SETF instruction.
    ///
    /// Sets ALU flags according to the given value.
    #[insn(opcode = 0x3D, subopcode = 0x05)]
    SETF(u8, u8),

    /// The MULU instruction.
    ///
    /// Performs a multiplication of two unsigned 16-bit values and
    /// stores the result.
    #[insn(opcode = 0xC0, subopcode = 0x00)]
    #[insn(opcode = 0xE0, subopcode = 0x00)]
    #[insn(opcode = 0xF0, subopcode = 0x00)]
    #[insn(opcode = 0xF1, subopcode = 0x00)]
    #[insn(opcode = 0xFD, subopcode = 0x00)]
    #[insn(opcode = 0xFF, subopcode = 0x00)]
    MULU(u8, u8),

    /// The MULS instruction.
    ///
    /// Performs a multiplication of two signed 16-bit values and
    /// stores the result.
    #[insn(opcode = 0xC0, subopcode = 0x01)]
    #[insn(opcode = 0xE0, subopcode = 0x01)]
    #[insn(opcode = 0xF0, subopcode = 0x01)]
    #[insn(opcode = 0xF1, subopcode = 0x01)]
    #[insn(opcode = 0xFD, subopcode = 0x01)]
    #[insn(opcode = 0xFF, subopcode = 0x01)]
    MULS(u8, u8),

    /// The SXT instruction.
    ///
    /// Sign-extends the low bits of a value.
    #[insn(opcode = 0xC0, subopcode = 0x02)]
    #[insn(opcode = 0xF0, subopcode = 0x02)]
    #[insn(opcode = 0xFD, subopcode = 0x02)]
    #[insn(opcode = 0xFF, subopcode = 0x02)]
    SXT(u8, u8),

    /// The EXTR instruction.
    ///
    /// Extracts an unsigned bitfield and stores it in the low bits
    /// of the destination register.
    #[insn(opcode = 0xC0, subopcode = 0x07)]
    #[insn(opcode = 0xE0, subopcode = 0x07)]
    #[insn(opcode = 0xFF, subopcode = 0x07)]
    EXTR(u8, u8),

    /// The EXTRS instruction.
    ///
    /// Extracts a signed bitfield and stores it in the low bits
    /// of the destination register.
    #[insn(opcode = 0xC0, subopcode = 0x03)]
    #[insn(opcode = 0xE0, subopcode = 0x03)]
    #[insn(opcode = 0xFF, subopcode = 0x03)]
    EXTRS(u8, u8),

    /// The BINS instruction.
    ///
    /// Inserts a bitfield into a register.
    #[insn(opcode = 0xC0, subopcode = 0x0B)]
    #[insn(opcode = 0xE0, subopcode = 0x0B)]
    BINS(u8, u8),

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

    /// The DIV instruction.
    ///
    /// Performs an unsigned 32-bit division and stores the result.
    /// Division by zero always results in `0xFFFFFFFF`.
    #[insn(opcode = 0xC0, subopcode = 0x0C)]
    #[insn(opcode = 0xE0, subopcode = 0x0C)]
    #[insn(opcode = 0xFF, subopcode = 0x0C)]
    DIV(u8, u8),

    /// The MOD instruction.
    ///
    /// Performs an unsigned 32-bit modulus operation and stores
    /// the result. Division by zero always results in the first
    /// source operand.
    #[insn(opcode = 0xC0, subopcode = 0x0D)]
    #[insn(opcode = 0xE0, subopcode = 0x0D)]
    #[insn(opcode = 0xFF, subopcode = 0x0D)]
    MOD(u8, u8),

    /// The SETP instruction.
    ///
    /// Sets a specific bit in the flags register to the highest
    /// bit of the first operand.
    #[insn(opcode = 0xF2, subopcode = 0x8)]
    #[insn(opcode = 0xFA, subopcode = 0x8)]
    SETP(u8, u8),

    /// The LD instructions.
    ///
    /// Loads a value from data segment to a register.
    #[insn(opcode = 0x10, subopcode = 0x08)]
    #[insn(opcode = 0x34, subopcode = 0x00)]
    #[insn(opcode = 0x3A, subopcode = 0x00)]
    #[insn(opcode = 0x3C, subopcode = 0x08)]
    LD(u8, u8),

    /// The ST instruction.
    ///
    /// Stores a value from a register in data segment.
    #[insn(opcode = 0x00, subopcode = 0x00)]
    #[insn(opcode = 0x30, subopcode = 0x01)]
    #[insn(opcode = 0x38, subopcode = 0x00)]
    #[insn(opcode = 0x38, subopcode = 0x01)]
    ST(u8, u8),

    /// The PUSH instruction.
    ///
    /// Pushes a value onto the stack and decrements the stack pointer
    /// by four.
    #[insn(opcode = 0xF9, subopcode = 0x00)]
    PUSH(u8, u8),

    /// The POP instruction.
    ///
    /// Pops a value off the stack and increments the stack pointer
    /// by four.
    #[insn(opcode = 0xF2, subopcode = 0x00)]
    POP(u8, u8),

    /// The ADJ instruction.
    ///
    /// Adjusts the stack pointer by adding a value to it.
    #[insn(opcode = 0xF4, subopcode = 0x30)]
    #[insn(opcode = 0xF5, subopcode = 0x30)]
    #[insn(opcode = 0xF9, subopcode = 0x01)]
    ADJ(u8, u8),

    /// The BRA instruction.
    ///
    /// Performs a conditional branch to a PC-relative offset
    /// if the given condition evaluates to true.
    #[insn(opcode = 0xF4, subopcode = 0x00)]
    #[insn(opcode = 0xF4, subopcode = 0x01)]
    #[insn(opcode = 0xF4, subopcode = 0x02)]
    #[insn(opcode = 0xF4, subopcode = 0x03)]
    #[insn(opcode = 0xF4, subopcode = 0x04)]
    #[insn(opcode = 0xF4, subopcode = 0x05)]
    #[insn(opcode = 0xF4, subopcode = 0x06)]
    #[insn(opcode = 0xF4, subopcode = 0x07)]
    #[insn(opcode = 0xF4, subopcode = 0x08)]
    #[insn(opcode = 0xF4, subopcode = 0x09)]
    #[insn(opcode = 0xF4, subopcode = 0x0A)]
    #[insn(opcode = 0xF4, subopcode = 0x0B)]
    #[insn(opcode = 0xF4, subopcode = 0x0C)]
    #[insn(opcode = 0xF4, subopcode = 0x0D)]
    #[insn(opcode = 0xF4, subopcode = 0x0E)]
    #[insn(opcode = 0xF4, subopcode = 0x10)]
    #[insn(opcode = 0xF4, subopcode = 0x11)]
    #[insn(opcode = 0xF4, subopcode = 0x12)]
    #[insn(opcode = 0xF4, subopcode = 0x13)]
    #[insn(opcode = 0xF4, subopcode = 0x14)]
    #[insn(opcode = 0xF4, subopcode = 0x15)]
    #[insn(opcode = 0xF4, subopcode = 0x16)]
    #[insn(opcode = 0xF4, subopcode = 0x17)]
    #[insn(opcode = 0xF4, subopcode = 0x18)]
    #[insn(opcode = 0xF4, subopcode = 0x19)]
    #[insn(opcode = 0xF4, subopcode = 0x1A)]
    #[insn(opcode = 0xF4, subopcode = 0x1B)]
    #[insn(opcode = 0xF4, subopcode = 0x1C)]
    #[insn(opcode = 0xF4, subopcode = 0x1D)]
    #[insn(opcode = 0xF4, subopcode = 0x1E)]
    #[insn(opcode = 0xF4, subopcode = 0x1F)]
    #[insn(opcode = 0xF5, subopcode = 0x00)]
    #[insn(opcode = 0xF5, subopcode = 0x01)]
    #[insn(opcode = 0xF5, subopcode = 0x02)]
    #[insn(opcode = 0xF5, subopcode = 0x03)]
    #[insn(opcode = 0xF5, subopcode = 0x04)]
    #[insn(opcode = 0xF5, subopcode = 0x05)]
    #[insn(opcode = 0xF5, subopcode = 0x06)]
    #[insn(opcode = 0xF5, subopcode = 0x07)]
    #[insn(opcode = 0xF5, subopcode = 0x08)]
    #[insn(opcode = 0xF5, subopcode = 0x09)]
    #[insn(opcode = 0xF5, subopcode = 0x0A)]
    #[insn(opcode = 0xF5, subopcode = 0x0B)]
    #[insn(opcode = 0xF5, subopcode = 0x0C)]
    #[insn(opcode = 0xF5, subopcode = 0x0D)]
    #[insn(opcode = 0xF5, subopcode = 0x0E)]
    #[insn(opcode = 0xF5, subopcode = 0x10)]
    #[insn(opcode = 0xF5, subopcode = 0x11)]
    #[insn(opcode = 0xF5, subopcode = 0x12)]
    #[insn(opcode = 0xF5, subopcode = 0x13)]
    #[insn(opcode = 0xF5, subopcode = 0x14)]
    #[insn(opcode = 0xF5, subopcode = 0x15)]
    #[insn(opcode = 0xF5, subopcode = 0x16)]
    #[insn(opcode = 0xF5, subopcode = 0x17)]
    #[insn(opcode = 0xF5, subopcode = 0x18)]
    #[insn(opcode = 0xF5, subopcode = 0x19)]
    #[insn(opcode = 0xF5, subopcode = 0x1A)]
    #[insn(opcode = 0xF5, subopcode = 0x1B)]
    #[insn(opcode = 0xF5, subopcode = 0x1C)]
    #[insn(opcode = 0xF5, subopcode = 0x1D)]
    #[insn(opcode = 0xF5, subopcode = 0x1E)]
    #[insn(opcode = 0xF5, subopcode = 0x1F)]
    BRA(u8, u8),

    /// The JMP instruction.
    ///
    /// Performs an unconditional branch to an absolute address.
    #[insn(opcode = 0xF4, subopcode = 0x20)]
    #[insn(opcode = 0xF5, subopcode = 0x20)]
    #[insn(opcode = 0xF9, subopcode = 0x04)]
    JMP(u8, u8),

    /// The CALL instruction.
    ///
    /// Performs an unconditional branch to an absolute address,
    /// pushing the return address onto the stack.
    #[insn(opcode = 0xF4, subopcode = 0x21)]
    #[insn(opcode = 0xF5, subopcode = 0x21)]
    #[insn(opcode = 0xF9, subopcode = 0x05)]
    CALL(u8, u8),

    /// The RET instruction.
    ///
    /// Returns from a previous CALL instruction by popping the
    /// return address off the stack and loading it into PC.
    #[insn(opcode = 0xF8, subopcode = 0x00)]
    RET(u8, u8),

    /// The EXIT instruction.
    ///
    /// Halts the processor and triggers an exit interrupt.
    #[insn(opcode = 0xF8, subopcode = 0x02)]
    EXIT(u8, u8),

    /// The SLEEP instruction.
    ///
    /// Puts the processor into a sleep state until an unmasked
    /// interrupt is received.
    #[insn(opcode = 0xF4, subopcode = 0x28)]
    SLEEP(u8, u8),

    /// The PTLB instruction.
    ///
    /// Loads the TLB that covers a given physical page into a
    /// destination register.
    #[insn(opcode = 0xFE, subopcode = 0x02)]
    PTLB(u8, u8),

    /// The VTLB instruction.
    ///
    /// Loads the TLB that covers a given virtual address into a
    /// destination register.
    #[insn(opcode = 0xFE, subopcode = 0x03)]
    VTLB(u8, u8),

    /// The ITLB instruction.
    ///
    /// Clears a non-secret TLB entry corresponding to a specified
    /// physical page.
    #[insn(opcode = 0xF9, subopcode = 0x08)]
    ITLB(u8, u8),

    /// The IRET instruction.
    ///
    /// Returns from an interrupt handler.
    #[insn(opcode = 0xF8, subopcode = 0x01)]
    IRET(u8, u8),

    /// The TRAP instruction.
    ///
    /// Triggers a software trap.
    #[insn(opcode = 0xF8, subopcode = 0x08)]
    #[insn(opcode = 0xF8, subopcode = 0x09)]
    #[insn(opcode = 0xF8, subopcode = 0x0A)]
    #[insn(opcode = 0xF8, subopcode = 0x0B)]
    TRAP(u8, u8),

    /// The XCLD instruction.
    ///
    /// Submits a DMA transfer request for a code load from
    /// external memory.
    #[insn(opcode = 0xFA, subopcode = 0x04)]
    XCLD(u8, u8),

    /// The XDLD instruction.
    ///
    /// Submits a DMA transfer request for a data load from
    /// external memory.
    #[insn(opcode = 0xFA, subopcode = 0x05)]
    XDLD(u8, u8),

    /// The XDST instruction.
    ///
    /// Submits a DMA transfer request for a data store from
    /// local Falcon data space to external memory.
    #[insn(opcode = 0xFA, subopcode = 0x06)]
    XDST(u8, u8),

    /// The XCWAIT instruction.
    ///
    /// Waits for all DMA code load transfers to complete.
    #[insn(opcode = 0xF8, subopcode = 0x07)]
    XCWAIT(u8, u8),

    /// The XDWAIT instruction.
    ///
    /// Waits for all DMA code load/store transfers to complete.
    #[insn(opcode = 0xF8, subopcode = 0x03)]
    XDWAIT(u8, u8),

    /// The IOWR instruction.
    ///
    /// Writes a word to the I/O space of the processor asynchronously.
    #[insn(opcode = 0xD0, subopcode = 0x00)]
    #[insn(opcode = 0xFA, subopcode = 0x00)]
    IOWR(u8, u8),

    /// The IOWRS instruction.
    ///
    /// Writes a word to the I/O space of the processor synchronously.
    #[insn(opcode = 0xD0, subopcode = 0x01)]
    #[insn(opcode = 0xFA, subopcode = 0x01)]
    IOWRS(u8, u8),

    /// The IORD instruction.
    ///
    /// Reads a word from the I/O space of the processor.
    #[insn(opcode = 0xCF, subopcode = 0x0F)]
    #[insn(opcode = 0xFF, subopcode = 0x0F)]
    IORD(u8, u8),

    /// An invalid or unknown instruction.
    ///
    /// This can be checked through calling [`InstructionKind::invalid`].
    /// If such an instruction is encountered, the user may halt the
    /// program.
    ///
    /// [`InstructionKind::invalid`]: enum.InstructionKind.html#method.invalid
    XXX,
}

impl fmt::Display for InstructionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mnemonic = match self {
            InstructionKind::ADC(_, _) => "adc",
            InstructionKind::ADD(_, _) => "add",
            InstructionKind::ADJ(_, _) => "adj",
            InstructionKind::AND(_, _) => "and",
            InstructionKind::BCLR(_, _) => "bclr",
            InstructionKind::BINS(_, _) => "bins",
            InstructionKind::BRA(_, 0x00) => "bra p0",
            InstructionKind::BRA(_, 0x01) => "bra p1",
            InstructionKind::BRA(_, 0x02) => "bra p2",
            InstructionKind::BRA(_, 0x03) => "bra p3",
            InstructionKind::BRA(_, 0x04) => "bra p4",
            InstructionKind::BRA(_, 0x05) => "bra p5",
            InstructionKind::BRA(_, 0x06) => "bra p6",
            InstructionKind::BRA(_, 0x07) => "bra p7",
            InstructionKind::BRA(_, 0x08) => "bra c",
            InstructionKind::BRA(_, 0x09) => "bra o",
            InstructionKind::BRA(_, 0x0A) => "bra s",
            InstructionKind::BRA(_, 0x0B) => "bra z",
            InstructionKind::BRA(_, 0x0C) => "bra a",
            InstructionKind::BRA(_, 0x0D) => "bra na",
            InstructionKind::BRA(_, 0x0E) => "bra",
            InstructionKind::BRA(_, 0x10) => "bra np0",
            InstructionKind::BRA(_, 0x11) => "bra np1",
            InstructionKind::BRA(_, 0x12) => "bra np2",
            InstructionKind::BRA(_, 0x13) => "bra np3",
            InstructionKind::BRA(_, 0x14) => "bra np4",
            InstructionKind::BRA(_, 0x15) => "bra np5",
            InstructionKind::BRA(_, 0x16) => "bra np6",
            InstructionKind::BRA(_, 0x17) => "bra np7",
            InstructionKind::BRA(_, 0x18) => "bra nc",
            InstructionKind::BRA(_, 0x19) => "bra no",
            InstructionKind::BRA(_, 0x1A) => "bra ns",
            InstructionKind::BRA(_, 0x1B) => "bra nz",
            InstructionKind::BRA(_, 0x1C) => "bra g",
            InstructionKind::BRA(_, 0x1D) => "bra le",
            InstructionKind::BRA(_, 0x1E) => "bra l",
            InstructionKind::BRA(_, 0x1F) => "bra ge",
            InstructionKind::BSET(_, _) => "bset",
            InstructionKind::BTGL(_, _) => "btgl",
            InstructionKind::CALL(_, _) => "call",
            InstructionKind::CLR(_, _) => "clr",
            InstructionKind::CMP(_, _) => "cmp",
            InstructionKind::CMPS(_, _) => "cmps",
            InstructionKind::CMPU(_, _) => "cmpu",
            InstructionKind::DIV(_, _) => "div",
            InstructionKind::EXIT(_, _) => "exit",
            InstructionKind::EXTR(_, _) => "extr",
            InstructionKind::EXTRS(_, _) => "extrs",
            InstructionKind::HSWAP(_, _) => "hswap",
            InstructionKind::IORD(_, _) => "iord",
            InstructionKind::IOWR(_, _) => "iowr",
            InstructionKind::IOWRS(_, _) => "iowrs",
            InstructionKind::IRET(_, _) => "iret",
            InstructionKind::ITLB(_, _) => "itlb",
            InstructionKind::JMP(_, _) => "jmp",
            InstructionKind::LD(_, _) => "ld",
            InstructionKind::LIM(_, _) => "lim",
            InstructionKind::MOD(_, _) => "mov",
            InstructionKind::MOV(_, _) => "mov",
            InstructionKind::MULS(_, _) => "muls",
            InstructionKind::MULU(_, _) => "mulu",
            InstructionKind::NEG(_, _) => "neg",
            InstructionKind::NOT(_, _) => "not",
            InstructionKind::OR(_, _) => "or",
            InstructionKind::POP(_, _) => "pop",
            InstructionKind::PTLB(_, _) => "ptlb",
            InstructionKind::PUSH(_, _) => "push",
            InstructionKind::RET(_, _) => "ret",
            InstructionKind::SBB(_, _) => "sbb",
            InstructionKind::SETF(_, _) => "setf",
            InstructionKind::SETHI(_, _) => "sethi",
            InstructionKind::SETP(_, _) => "setp",
            InstructionKind::SHL(_, _) => "shl",
            InstructionKind::SHLC(_, _) => "shlc",
            InstructionKind::SHR(_, _) => "shr",
            InstructionKind::SHRC(_, _) => "shrc",
            InstructionKind::SUB(_, _) => "sub",
            InstructionKind::TRAP(_, _) => "trap",
            InstructionKind::XBIT(_, _) => "xbit",
            InstructionKind::XCLD(_, _) => "xcld",
            InstructionKind::XCWAIT(_, _) => "xcwait",
            InstructionKind::XDLD(_, _) => "xdld",
            InstructionKind::XDST(_, _) => "xdst",
            InstructionKind::XDWAIT(_, _) => "xdwait",
            InstructionKind::XOR(_, _) => "xor",
            _ => "???",
        };

        write!(f, "{}", mnemonic)
    }
}

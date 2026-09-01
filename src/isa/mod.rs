//! Defines the Falcon Instruction Set Architecture.
//!
//! The [`Instruction`] type models a Falcon machine instruction
//! as a convenient Rust abstraction with support for encoding
//! and decoding it to and from machine code.

mod lookup;
pub use lookup::decode;

mod opcode;

mod operands;

/// Instruction opcodes in Falcon assembly.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Opcode {
    CMPU,
    CMPS,
    CMP,
    ADD,
    CCR,
    ADC,
    SUB,
    SBB,
    SHL,
    SHR,
    SAR,
    SHLC,
    SHRC,
    NOT,
    NEG,
    HSWAP,
    SETHI,
    CLEAR,
    TEST,
    MULU,
    MULS,
    SEXT,
    AND,
    OR,
    XOR,
    XBIT,
    BSET,
    BCLR,
    BTGL,
    DIV,
    MOD,
    SETP,
    EXTR,
    EXTRS,
    INS,
    MOV,
    LD,
    ST,
    PUSH,
    POP,
    MPUSH,
    MPOP,
    MPOPADD,
    MPOPRET,
    MPOPADDRET,
    CALL,
    LCALL,
    JMP,
    LJMP,
    BP,
    BC,
    BO,
    BS,
    BZ,
    BA,
    BNA,
    BRA,
    BNP,
    BNC,
    BNO,
    BNS,
    BNZ,
    BG,
    BLE,
    BL,
    BGE,
    BCMPE,
    BCMPNE,
    RET,
    HALT,
    SLEEP,
    IMBLK,
    IMTAG,
    IMINV,
    IRET,
    TRAP,
    IMLD,
    DMLD,
    DMST,
    IMWAIT,
    DMWAIT,
    DMFENCE,
    IOWR,
    IOWRS,
    IORD,
    IORDS,
}

/// Represents the operand size of an instruction.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperandSize {
    /// An 8-bit operand mode.
    EightBit,
    /// A 16-bit operand mode.
    SixteenBit,
    /// A 32-bit operand mode.
    ThirtyTwoBit,
    /// Unsized operand mode.
    Unsized,
}

/// A 32-bit immediate value (signed or unsigned).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Immediate {
    /// The actual value.
    pub value: u32,
    /// Whether the value should be interpreted as signed.
    pub signed: bool,
}

/// Types of supported processor registers.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegisterKind {
    /// A general-purpose register.
    Gpr,
    /// A special-purpose register.
    Spr,
}

/// Represents a Falcon processor register.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Register {
    /// The type of register.
    pub kind: RegisterKind,
    /// Index value of the register.
    pub index: u8,
}

/// Memory spaces accessible to instructions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemorySpace {
    /// The I/O space.
    Io,
    /// The data space (DMEM).
    Data,
}

/// Represents an encoded memory address to be accessed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MemoryAccess {
    /// The memory space that is accessed.
    pub space: MemorySpace,
    /// The register storing the base address.
    pub base: Register,
    /// A variable offset applied to the base address.
    pub offset: MemoryOffset,
}

/// Describes an offset from the base address of a memory access.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemoryOffset {
    /// No offset is applied.
    None,
    /// Offset is the index register value multiplied by scale.
    Reg { index: Register, scale: u8 },
    /// Offset is a fixed displacement value added to base.
    Imm(u32),
}

/// Describes a flag bit in the `$csw` register by index.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Flag(pub u8);

/// Describes a span of bits `[start, end)`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BitRange {
    /// The start index (inclusive) of the bit range.
    pub start: u8,
    /// The end index (exclusive) of the bit range.
    pub end: u8,
}

/// An instruction operand.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operand {
    /// An empty placeholder operand.
    None,
    /// Immediate operand.
    Imm(Immediate),
    /// Register operand.
    Reg(Register),
    /// Flag bit operand.
    Flag(Flag),
    /// Memory access operand.
    Mem(MemoryAccess),
    /// Bit range operand.
    Bits(BitRange),
    /// PC-relative address operand.
    PcRel(Immediate),
}

/// A Falcon processor instruction.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Instruction {
    pub size: OperandSize,
    pub opcode: Opcode,
    pub operands: [Operand; 3],
}

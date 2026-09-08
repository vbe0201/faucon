//! Defines the Falcon Instruction Set Architecture.
//!
//! The [`Instruction`] type models a Falcon machine instruction
//! as a convenient Rust abstraction with support for encoding
//! and decoding it to and from machine code.

use crate::str_enum;

mod lookup;
pub use lookup::decode;

mod opcode;

mod operands;

str_enum! {
    /// Instruction opcodes in Falcon assembly.
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Opcode {
        CMPU = "cmpu",
        CMPS = "cmps",
        CMP = "cmp",
        ADD = "add",
        CCR = "ccr",
        ADC = "adc",
        SUB = "sub",
        SBB = "sbb",
        SHL = "shl",
        SHR = "shr",
        SAR = "sar",
        SHLC = "shlc",
        SHRC = "shrc",
        NOT = "not",
        NEG = "neg",
        HSWAP = "hswap",
        SETHI = "sethi",
        CLEAR = "clear",
        TEST = "test",
        MULU = "mulu",
        MULS = "muls",
        SEXT = "sext",
        AND = "and",
        OR = "or",
        XOR = "xor",
        XBIT = "xbit",
        BSET = "bset",
        BCLR = "bclr",
        BTGL = "btgl",
        DIV = "div",
        MOD = "mod",
        SETP = "setp",
        EXTR = "extr",
        EXTRS = "extrs",
        INS = "ins",
        MOV = "mov",
        LD = "ld",
        ST = "st",
        PUSH = "push",
        POP = "pop",
        MPUSH = "mpush",
        MPOP = "mpop",
        MPOPADD = "mpopadd",
        MPOPRET = "mpopret",
        MPOPADDRET = "mpopaddret",
        CALL = "call",
        LCALL = "lcall",
        JMP = "jmp",
        LJMP = "ljmp",
        BP = "bp",
        BC = "bc",
        BO = "bo",
        BS = "bs",
        BZ = "bz",
        BA = "ba",
        BNA = "bna",
        BRA = "bra",
        BNP = "bnp",
        BNC = "bnc",
        BNO = "bno",
        BNS = "bns",
        BNZ = "bnz",
        BG = "bg",
        BLE = "ble",
        BL = "bl",
        BGE = "bge",
        BCMPE = "bcmpe",
        BCMPNE = "bcmpne",
        RET = "ret",
        HALT = "halt",
        SLEEP = "sleep",
        IMBLK = "imblk",
        IMTAG = "imtag",
        IMINV = "iminv",
        IRET = "iret",
        TRAP = "trap",
        IMLD = "imld",
        DMLD = "dmld",
        DMST = "dmst",
        IMWAIT = "imwait",
        DMWAIT = "dmwait",
        DMFENCE = "dmfence",
        IOWR = "iowr",
        IOWRS = "iowrs",
        IORD = "iord",
        IORDS = "iords",
    }
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

impl Register {
    /// General-purpose register 0.
    pub const REG0: Self = Self::gpr(0);
    /// General-purpose register 1.
    pub const REG1: Self = Self::gpr(1);
    /// General-purpose register 2.
    pub const REG2: Self = Self::gpr(2);
    /// General-purpose register 3.
    pub const REG3: Self = Self::gpr(3);
    /// General-purpose register 4.
    pub const REG4: Self = Self::gpr(4);
    /// General-purpose register 5.
    pub const REG5: Self = Self::gpr(5);
    /// General-purpose register 6.
    pub const REG6: Self = Self::gpr(6);
    /// General-purpose register 7.
    pub const REG7: Self = Self::gpr(7);
    /// General-purpose register 8.
    pub const REG8: Self = Self::gpr(8);
    /// General-purpose register 9.
    pub const REG9: Self = Self::gpr(9);
    /// General-purpose register 10.
    pub const REG10: Self = Self::gpr(10);
    /// General-purpose register 11.
    pub const REG11: Self = Self::gpr(11);
    /// General-purpose register 12.
    pub const REG12: Self = Self::gpr(12);
    /// General-purpose register 13.
    pub const REG13: Self = Self::gpr(13);
    /// General-purpose register 14.
    pub const REG14: Self = Self::gpr(14);
    /// General-purpose register 15.
    pub const REG15: Self = Self::gpr(15);

    /// Holds the address for interrupt vector 0.
    pub const IV0: Self = Self::spr(0);
    /// Holds the address for interrupt vector 1.
    pub const IV1: Self = Self::spr(1);
    /// Holds the address for interrupt vector 2.
    pub const IV2: Self = Self::spr(2);
    /// Holds the address for the exception vector.
    pub const EV: Self = Self::spr(3);
    /// Holds the stack pointer.
    pub const SP: Self = Self::spr(4);
    /// Holds the program counter.
    pub const PC: Self = Self::spr(5);
    /// Holds the external base address for IMEM transfers.
    pub const IMB: Self = Self::spr(6);
    /// Holds the external base address for DMEM transfers.
    pub const DMB: Self = Self::spr(7);
    /// Flags register.
    pub const CSW: Self = Self::spr(8);
    /// Configuration register for DMA overrides.
    pub const CCR: Self = Self::spr(9);
    /// Configuration register for SCP authentication.
    pub const SEC: Self = Self::spr(10);
    /// Configuration register for CTXDMA ports.
    pub const CTX: Self = Self::spr(11);
    /// Holds information on raised exceptions.
    pub const EXCI: Self = Self::spr(12);
    /// Reserved register.
    pub const SEC1: Self = Self::spr(13);
    /// Reserved register.
    pub const IMB1: Self = Self::spr(14);
    /// Reserved register.
    pub const DMB1: Self = Self::spr(15);

    const fn gpr(index: u8) -> Self {
        Self {
            kind: RegisterKind::Gpr,
            index,
        }
    }

    const fn spr(index: u8) -> Self {
        Self {
            kind: RegisterKind::Spr,
            index,
        }
    }
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

impl Flag {
    /// General-purpose predicate 0.
    pub const P0: Self = Self(0);
    /// General-purpose predicate 1.
    pub const P1: Self = Self(1);
    /// General-purpose predicate 2.
    pub const P2: Self = Self(2);
    /// General-purpose predicate 3.
    pub const P3: Self = Self(3);
    /// General-purpose predicate 4.
    pub const P4: Self = Self(4);
    /// General-purpose predicate 5.
    pub const P5: Self = Self(5);
    /// General-purpose predicate 6.
    pub const P6: Self = Self(6);
    /// General-purpose predicate 7.
    pub const P7: Self = Self(7);
    /// ALU carry flag.
    pub const C: Self = Self(8);
    /// ALU signed overflow flag.
    pub const O: Self = Self(9);
    /// ALU sign flag.
    pub const S: Self = Self(10);
    /// ALU zero flag.
    pub const Z: Self = Self(11);
    /// Interrupt 0 enable.
    pub const IE0: Self = Self(16);
    /// Interrupt 1 enable.
    pub const IE1: Self = Self(17);
    /// Interrupt 2 enable.
    pub const IE2: Self = Self(18);
    /// Interrupt 0 saved enable.
    pub const IS0: Self = Self(20);
    /// Interrupt 1 saved enable.
    pub const IS1: Self = Self(21);
    /// Interrupt 2 saved enable.
    pub const IS2: Self = Self(22);
    /// Exception active.
    pub const EA: Self = Self(24);
}

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

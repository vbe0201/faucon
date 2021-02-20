//! A parser layer around actual Falcon operands, describing their position, size
//! and representation.

use std::cmp::max;

use byteorder::{ByteOrder, LittleEndian};
use num_traits::{cast, NumCast, PrimInt};

use crate::operands::{MemorySpace, RegisterKind};

// A helper macro that is supposed to unwrap an `Argument` of a known kind into
// its contained type, may it be a `Register`, `Immediate` or `MemoryAccess`.
//
// This is useful when operands encode each other, e.g. a memory access encodes
// a specific register argument, to remove the boilerplate code of validating a
// variant of `Argument` at runtime by feeding that directly into the constant
// evaluation engine.
macro_rules! unwrap {
    ($argument:expr, $variant:pat => $result:expr) => {
        if let $variant = $argument {
            $result
        } else {
            // SAFETY: As an internal macro that is only applied on constant
            // `Argument` instances with known variants, it always produces the
            // expected results and code flow never gets to land here.
            unsafe { ::std::hint::unreachable_unchecked() }
        }
    };
}

/// An unsigned 8-bit immediate.
///
/// These are used for bit positions, shifts and 8-bit instructions.
pub const I8: Argument = Argument::U8(Immediate {
    position: 2,
    width: 1,
    sign: false,
    shift: None,
    mask: None,
    raw_value: None,
});

/// An unsigned 8-bit immediate zero-extended to 16 bits.
///
/// These are used for sethi and 16-bit instructions.
pub const I8ZX16: Argument = Argument::U16(Immediate {
    position: 2,
    width: 1,
    sign: false,
    shift: None,
    mask: None,
    raw_value: None,
});

/// A signed 8-bit immediate sign-extended to 16 bits.
///
/// These are used for sethi and 16-bit instructions.
pub const I8SX16: Argument = Argument::I16(Immediate {
    position: 2,
    width: 1,
    sign: true,
    shift: None,
    mask: None,
    raw_value: None,
});

/// An unsigned 8-bit immediate zero-extended to 32 bits.
///
/// These are used for memory addressing and most 32-bit instructions.
pub const I8ZX32: Argument = Argument::U32(Immediate {
    position: 2,
    width: 1,
    sign: false,
    shift: None,
    mask: None,
    raw_value: None,
});

/// A signed 32-bit immediate sign-extended to 32 bits.
///
/// These are used for memory addressing and most 32-bit instructions.
pub const I8SX32: Argument = Argument::I32(Immediate {
    position: 2,
    width: 1,
    sign: true,
    shift: None,
    mask: None,
    raw_value: None,
});

/// A signed 8-bit immediate sign-extended to 32 bits.
///
/// These are used for Falcon v5 MOV instructions.
pub const I8SX32P1: Argument = Argument::I32(Immediate {
    position: 1,
    width: 1,
    sign: true,
    shift: None,
    mask: None,
    raw_value: None,
});

/// An unsigned 8-bit immediate zero-extended to 32 bits and shifted left
/// by one.
///
/// These are mainly used for memory addressing.
pub const I8ZX32S1: Argument = Argument::U32(Immediate {
    position: 2,
    width: 1,
    sign: false,
    shift: Some(1),
    mask: None,
    raw_value: None,
});

/// An unsigned 8-bit immediate zero-extended to 32 bits and shifted left
/// by two.
///
/// These are mainly used for memory addressing.
pub const I8ZX32S2: Argument = Argument::U32(Immediate {
    position: 2,
    width: 1,
    sign: false,
    shift: Some(2),
    mask: None,
    raw_value: None,
});

/// An unsigned 8-bit immediate zero-extended to 32 bits and shifted left
/// by 16.
///
/// These are used by the SETHI instruction.
pub const I8ZX32S16: Argument = Argument::U32(Immediate {
    position: 2,
    width: 1,
    sign: false,
    shift: Some(16),
    mask: None,
    raw_value: None,
});

/// A helper that leverages the selection of a correct parser for immediates
/// in sized instructions to the disassembler.
///
/// This handles zero-extension for the different operand sizes.
pub const I8ZXS: Argument = Argument::SizeConverter(|size| match size {
    0 => I8,
    1 => I8ZX16,
    2 => I8ZX32,
    _ => unreachable!(),
});

/// A helper that leverages the selection of an appropriate parser for immediates
/// in sized instructions to the disassembler.
///
/// This handles sign-extension for the different operand sizes.
pub const I8SXS: Argument = Argument::SizeConverter(|size| match size {
    0 => I8,
    1 => I8SX16,
    2 => I8SX32,
    _ => unreachable!(),
});

/// An unsigned 16-bit immediate truncated to the low 8 bits.
///
/// Used by 8-bit instructions which have a 16-bit immediate form for
/// whatever reason.
pub const I16T8: Argument = Argument::U8(Immediate {
    position: 2,
    width: 2,
    sign: false,
    shift: None,
    mask: Some(0xFF),
    raw_value: None,
});

/// An unsigned 16-bit immediate.
///
/// These are used by sethi and 16-bit instructions.
pub const I16: Argument = Argument::U16(Immediate {
    position: 2,
    width: 2,
    sign: false,
    shift: None,
    mask: None,
    raw_value: None,
});

/// An unsigned 16-bit immediate zero-extended to 32 bits.
///
/// These are used for most 32-bit instructions.
pub const I16ZX32: Argument = Argument::U32(Immediate {
    position: 2,
    width: 2,
    sign: false,
    shift: None,
    mask: None,
    raw_value: None,
});

/// An unsigned 16-bit immediate zero-extended to 32 bits.
///
/// These are used for Falcon v5 call instructions.
pub const I16ZX32P1: Argument = Argument::U32(Immediate {
    position: 1,
    width: 2,
    sign: false,
    shift: None,
    mask: None,
    raw_value: None,
});

/// A signed 16-bit immediate sign-extended to 32 bits.
///
/// These are used for most 32-bit instructions.
pub const I16SX32: Argument = Argument::I32(Immediate {
    position: 2,
    width: 2,
    sign: true,
    shift: None,
    mask: None,
    raw_value: None,
});

/// A signed 16-bit immediate sign-extended to 32 bits.
///
/// These are used for Falcon v5 MOV instructions.
pub const I16SX32P1: Argument = Argument::I32(Immediate {
    position: 1,
    width: 2,
    sign: true,
    shift: None,
    mask: None,
    raw_value: None,
});

/// A helper that leverages the selection of a correct parser for immediates
/// in sized instructions to the disassembler.
///
/// This handles zero-extension for the different operand sizes.
pub const I16ZXS: Argument = Argument::SizeConverter(|size| match size {
    0 => I16T8,
    1 => I16,
    2 => I16ZX32,
    _ => unreachable!(),
});

/// A helper that leverages the selection of an appropriate parser for immediates
/// in sized instructions to the disassembler.
///
/// This handles sign-extension for the different operand sizes.
pub const I16SXS: Argument = Argument::SizeConverter(|size| match size {
    0 => I16T8,
    1 => I16,
    2 => I16SX32,
    _ => unreachable!(),
});

/// An unsigned 24-bit immediate zero-extended to 32 bits.
///
/// These are used for absolute call/jump addresses.
pub const I24ZX32: Argument = Argument::U32(Immediate {
    position: 1,
    width: 3,
    sign: false,
    shift: None,
    mask: None,
    raw_value: None,
});

/// A signed 24-bit immediate sign-extended to 32 bits.
///
/// These are used for Falcon v5 CALL instructions.
pub const I24SX32: Argument = Argument::I32(Immediate {
    position: 1,
    width: 3,
    sign: true,
    shift: None,
    mask: None,
    raw_value: None,
});

/// An unsigned 32-bit immediate.
///
/// These are used for MOV instructions.
pub const I32: Argument = Argument::U32(Immediate {
    position: 1,
    width: 4,
    sign: false,
    shift: None,
    mask: None,
    raw_value: None,
});

/// A Falcon general-purpose register, encoded in the low 4 bits of the first
/// instruction byte.
pub const R0: Argument = Argument::Register(Register {
    kind: RegisterKind::Gpr,
    position: 0,
    high: false,
    raw_value: None,
});

/// A Falcon general-purpose register, encoded in the low 4 bits of the second
/// instruction byte.
pub const R1: Argument = Argument::Register(Register {
    kind: RegisterKind::Gpr,
    position: 1,
    high: false,
    raw_value: None,
});

/// A Falcon general-purpose register, encoded in the high 4 bits of the second
/// instruction byte.
pub const R2: Argument = Argument::Register(Register {
    kind: RegisterKind::Gpr,
    position: 1,
    high: true,
    raw_value: None,
});

/// A Falcon general-purpose register, encoded in the high 4 bits of the third
/// instruction byte.
pub const R3: Argument = Argument::Register(Register {
    kind: RegisterKind::Gpr,
    position: 2,
    high: true,
    raw_value: None,
});

/// The stack pointer register.
///
/// It is used for instructions that operate on $sp by default, without
/// encoding its value in the instruction bytes.
pub const SP: Argument = Argument::Register(Register {
    kind: RegisterKind::Spr,
    position: 0,
    high: false,
    raw_value: Some(4),
});

/// The CPU flags register.
///
/// It is used for instructions that operate on $flags by default, without
/// encoding its value in the instruction bytes.
pub const FLAGS: Argument = Argument::Register(Register {
    kind: RegisterKind::Spr,
    position: 0,
    high: false,
    raw_value: Some(8),
});

/// A selected bit in the Falcon $flags register.
///
/// Treated as an 8-bit immediate by the hardware, used for miscellaneous
/// instructions that operate on the flag bits.
pub const FLAG: Argument = Argument::Flag(Immediate {
    position: 2,
    width: 1,
    sign: false,
    shift: None,
    mask: Some(0x1F),
    raw_value: None,
});

/// A software trap value.
///
/// It is used by the TRAP instruction and is encoded in the low two bits of
/// instruction byte 1, the subopcode in this case.
pub const TRAP: Argument = Argument::U8(Immediate {
    position: 1,
    width: 1,
    sign: false,
    shift: None,
    mask: Some(0b11),
    raw_value: None,
});

/// A Falcon special-purpose register, encoded in the high 4 bits of the second
/// instruction byte.
pub const SR1: Argument = Argument::Register(Register {
    kind: RegisterKind::Spr,
    position: 1,
    high: true,
    raw_value: None,
});

/// A Falcon special-purpose register, encoded in the low 4 bits of the second
/// instruction byte.
pub const SR2: Argument = Argument::Register(Register {
    kind: RegisterKind::Spr,
    position: 1,
    high: false,
    raw_value: None,
});

/// A memory access to an 8-bit value in Falcon DMem. The address is stored in a single
/// register.
pub const MEMR8: Argument = Argument::Memory(MemoryAccess::Reg(
    MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
));

/// A memory access to a 16-bit value in Falcon DMem. The address is stored in a single
/// register.
pub const MEMR16: Argument = Argument::Memory(MemoryAccess::Reg(
    MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
));

/// A memory access to a 32-bit value in Falcon DMem. The address is stored in a single
/// register.
pub const MEMR32: Argument = Argument::Memory(MemoryAccess::Reg(
    MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
));

/// A helper that leverages the selection of an appropriate parser for memory access
/// encodings in sized instructions to the disassembler.
pub const MEMR: Argument = Argument::SizeConverter(|size| match size {
    0 => MEMR8,
    1 => MEMR16,
    2 => MEMR32,
    _ => unreachable!(),
});

/// A memory access to an 8-bit value in Falcon DMem. The address is composed from a
/// base address in a register and an immediate offset.
pub const MEMRI8: Argument = Argument::Memory(MemoryAccess::RegImm(
    MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(I8ZX32, Argument::U32(imm) => imm),
));

/// A memory access to a 16-bit value in Falcon DMem. The address is composed from a
/// base address in a register and an immediate offset.
pub const MEMRI16: Argument = Argument::Memory(MemoryAccess::RegImm(
    MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(I8ZX32S1, Argument::U32(imm) => imm),
));

/// A memory access to a 32-bit value in Falcon DMem. The address is composed from a
/// base address in a register and an immediate offset.
pub const MEMRI32: Argument = Argument::Memory(MemoryAccess::RegImm(
    MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(I8ZX32S2, Argument::U32(imm) => imm),
));

/// A helper that leverages the selection of an appropriate parser for memory access
/// encodings in sized instructions to the disassembler.
pub const MEMRI: Argument = Argument::SizeConverter(|size| match size {
    0 => MEMRI8,
    1 => MEMRI16,
    2 => MEMRI32,
    _ => unreachable!(),
});

/// A memory access to an 8-bit value in Falcon DMem. The address is composed from a
/// base address in the `$sp` register and an immediate offset.
pub const MEMSPI8: Argument = Argument::Memory(MemoryAccess::RegImm(
    MemorySpace::DMem,
    unwrap!(SP, Argument::Register(r) => r),
    unwrap!(I8ZX32, Argument::U32(imm) => imm),
));

/// A memory access to a 16-bit value in Falcon DMem. The address is composed from a
/// base address in the `$sp` register and an immediate offset.
pub const MEMSPI16: Argument = Argument::Memory(MemoryAccess::RegImm(
    MemorySpace::DMem,
    unwrap!(SP, Argument::Register(r) => r),
    unwrap!(I8ZX32S1, Argument::U32(imm) => imm),
));

/// A memory access to a 32-bit value in Falcon DMem. The address is composed from a
/// base address in the `$sp` register and an immediate offset.
pub const MEMSPI32: Argument = Argument::Memory(MemoryAccess::RegImm(
    MemorySpace::DMem,
    unwrap!(SP, Argument::Register(r) => r),
    unwrap!(I8ZX32S2, Argument::U32(imm) => imm),
));

/// A helper that leverages the selection of an appropriate parser for memory access
/// encodings in sized instructions to the disassembler.
pub const MEMSPI: Argument = Argument::SizeConverter(|size| match size {
    0 => MEMSPI8,
    1 => MEMSPI16,
    2 => MEMSPI32,
    _ => unreachable!(),
});

/// A memory access to an 8-bit value in Falcon DMem. The address is composed from a
/// base address in the `$sp` register and an offset in another register.
pub const MEMSPR8: Argument = Argument::Memory(MemoryAccess::RegReg(
    MemorySpace::DMem,
    unwrap!(SP, Argument::Register(r) => r),
    unwrap!(R1, Argument::Register(r) => r),
    1,
));

/// A memory access to a 16-bit value in Falcon DMem. The address is composed from a
/// base address in the `$sp` register and an offset * 2 in another register.
pub const MEMSPR16: Argument = Argument::Memory(MemoryAccess::RegReg(
    MemorySpace::DMem,
    unwrap!(SP, Argument::Register(r) => r),
    unwrap!(R1, Argument::Register(r) => r),
    2,
));

/// A memory access to a 32-bit value in Falcon DMem. The address is composed from a
/// base address in the `$sp` register and an offset * 4 in another register.
pub const MEMSPR32: Argument = Argument::Memory(MemoryAccess::RegReg(
    MemorySpace::DMem,
    unwrap!(SP, Argument::Register(r) => r),
    unwrap!(R1, Argument::Register(r) => r),
    4,
));

/// A helper that leverages the selection of an appropriate parser for memory access
/// encodings in sized instructions to the disassembler.
pub const MEMSPR: Argument = Argument::SizeConverter(|size| match size {
    0 => MEMSPR8,
    1 => MEMSPR16,
    2 => MEMSPR32,
    _ => unreachable!(),
});

/// A memory access to an 8-bit value in Falcon DMem. The address is composed from a
/// base address in a register and an offset in another register.
pub const MEMRR8: Argument = Argument::Memory(MemoryAccess::RegReg(
    MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(R1, Argument::Register(r) => r),
    1,
));

/// A memory access to a 16-bit value in Falcon DMem. The address is composed from a
/// base address in a register and an offset * 2 in another register.
pub const MEMRR16: Argument = Argument::Memory(MemoryAccess::RegReg(
    MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(R1, Argument::Register(r) => r),
    2,
));

/// A memory access to a 32-bit value in Falcon DMem. The address is composed from a
/// base address in a register and an offset * 4 in another register.
pub const MEMRR32: Argument = Argument::Memory(MemoryAccess::RegReg(
    MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(R1, Argument::Register(r) => r),
    4,
));

/// A helper that leverages the selection of an appropriate parser for memory access
/// encodings in sized instructions to the disassembler.
pub const MEMRR: Argument = Argument::SizeConverter(|size| match size {
    0 => MEMRR8,
    1 => MEMRR16,
    2 => MEMRR32,
    _ => unreachable!(),
});

/// A memory access to an 8-bit value in Falcon DMem. The address is composed from a
/// base address in a register and an offset in another register.
pub const MEMRRALT8: Argument = Argument::Memory(MemoryAccess::RegReg(
    MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(R3, Argument::Register(r) => r),
    1,
));

/// A memory access to a 16-bit value in Falcon DMem. The address is composed from a
/// base address in a register and an offset * 2 in another register.
pub const MEMRRALT16: Argument = Argument::Memory(MemoryAccess::RegReg(
    MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(R3, Argument::Register(r) => r),
    2,
));

/// A memory access to a 32-bit value in Falcon DMem. The address is composed from a
/// base address in a register and an offset * 4 in another register.
pub const MEMRRALT32: Argument = Argument::Memory(MemoryAccess::RegReg(
    MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(R3, Argument::Register(r) => r),
    4,
));

/// A helper that leverages the selection of an appropriate parser for memory access
/// encodings in sized instructions to the disassembler.
pub const MEMRRALT: Argument = Argument::SizeConverter(|size| match size {
    0 => MEMRRALT8,
    1 => MEMRRALT16,
    2 => MEMRRALT32,
    _ => unreachable!(),
});

/// A memory access to a 32-bit value in Falcon IMem. The address is specified by a
/// single register.
pub const IOR: Argument = Argument::Memory(MemoryAccess::Reg(
    MemorySpace::IMem,
    unwrap!(R2, Argument::Register(r) => r),
));

/// A memory access to a 32-bit value in Falcon IMem. The address is composed from a
/// base address in a register and an offset * 4 in another register.
pub const IORR: Argument = Argument::Memory(MemoryAccess::RegReg(
    MemorySpace::IMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(R1, Argument::Register(r) => r),
    4,
));

/// A memory access to a 32-bit value in Falcon IMem. The address is composed from a
/// base address in a register and an immediate offset.
pub const IORI: Argument = Argument::Memory(MemoryAccess::RegImm(
    MemorySpace::IMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(I8ZX32S2, Argument::U32(imm) => imm),
));

/// Wrapper around Falcon instruction operands.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Argument {
    /// An incredibly stupid hack to obtain variable operands parsers based on
    /// the operand size argument.
    ///
    /// This is used for sized instructions where immediates need to be extended
    /// differently for each size.
    SizeConverter(fn(size: u8) -> Argument),

    /// An unsigned 8-bit immediate.
    U8(Immediate<u8>),
    /// A signed 8-bit immediate.
    I8(Immediate<i8>),
    /// An unsigned 16-bit immediate.
    U16(Immediate<u16>),
    /// A signed 16-bit immediate.
    I16(Immediate<i16>),
    /// An unsigned 24-bit immediate.
    U24(Immediate<u32>),
    /// A signed 24-bit immediate.
    I24(Immediate<i32>),
    /// An unsigned 32-bit immediate.
    U32(Immediate<u32>),
    /// A signed 32-bit immediate.
    I32(Immediate<i32>),

    /// A CPU register.
    Register(Register),
    /// A flag bit in the `$flags` register.
    Flag(Immediate<u8>),

    /// A direct memory access to an address in a specific SRAM space.
    Memory(MemoryAccess),
}

impl Argument {
    /// Gets the position in the instruction bytes where an operand matching the
    /// [`Argument`] starts.
    ///
    /// [`Argument`]: enum.Argument.html
    pub fn position(&self) -> usize {
        match self {
            Argument::U8(imm) => imm.position,
            Argument::I8(imm) => imm.position,
            Argument::U16(imm) => imm.position,
            Argument::I16(imm) => imm.position,
            Argument::U24(imm) => imm.position,
            Argument::I24(imm) => imm.position,
            Argument::U32(imm) => imm.position,
            Argument::I32(imm) => imm.position,
            Argument::Register(reg) => reg.position,
            Argument::Flag(imm) => imm.position,
            Argument::Memory(mem) => mem.position(),
            Argument::SizeConverter(_) => unreachable!(),
        }
    }

    /// Gets the width over how many bytes an operand matching the [`Argument`]
    /// spans.
    ///
    /// [`Argument`]: enum.Argument.html
    pub fn width(&self) -> usize {
        match self {
            Argument::U8(imm) => imm.width,
            Argument::I8(imm) => imm.width,
            Argument::U16(imm) => imm.width,
            Argument::I16(imm) => imm.width,
            Argument::U24(imm) => imm.width,
            Argument::I24(imm) => imm.width,
            Argument::U32(imm) => imm.width,
            Argument::I32(imm) => imm.width,
            Argument::Register(_) => 1,
            Argument::Flag(imm) => imm.width,
            Argument::Memory(mem) => mem.width(),
            Argument::SizeConverter(_) => unreachable!(),
        }
    }
}

/// An immediate number in Falcon assembly.
///
/// Immediates can either carry metadata to parse them from instruction bytes, or
/// a value for immediates that aren't actually encoded in instruction bytes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Immediate<T> {
    position: usize,
    width: usize,
    sign: bool,
    shift: Option<usize>,
    mask: Option<usize>,

    raw_value: Option<T>,
}

impl<T: PrimInt + NumCast> Immediate<T> {
    fn shift(&self) -> usize {
        self.shift.unwrap_or(0)
    }

    fn mask(&self) -> usize {
        let value = match self.width {
            1 => 0xFF,
            2 => 0xFFFF,
            3 => 0xFFFFFF,
            4 => 0xFFFFFFFF,
            _ => panic!("Unsupported width argument supplied"),
        };

        self.mask.unwrap_or(value)
    }

    /// Reads the value that is represented by this [`Immediate`] from the
    /// given instruction bytes.
    ///
    /// [`Immediate`]: struct.Immediate.html
    pub fn read(&self, insn: &[u8]) -> T {
        if let Some(value) = self.raw_value {
            return value;
        }

        let value: T = match self.width {
            1 => {
                if self.sign {
                    cast(insn[self.position] as i8 & self.mask() as i8).unwrap()
                } else {
                    cast(insn[self.position] & self.mask() as u8).unwrap()
                }
            }
            2 => {
                if self.sign {
                    cast(LittleEndian::read_i16(&insn[self.position..]) & self.mask() as i16)
                        .unwrap()
                } else {
                    cast(LittleEndian::read_u16(&insn[self.position..]) & self.mask() as u16)
                        .unwrap()
                }
            }
            3 => {
                if self.sign {
                    cast(LittleEndian::read_i24(&insn[self.position..]) & self.mask() as i32)
                        .unwrap()
                } else {
                    cast(LittleEndian::read_u24(&insn[self.position..]) & self.mask() as u32)
                        .unwrap()
                }
            }
            4 => {
                if self.sign {
                    cast(LittleEndian::read_i32(&insn[self.position..]) & self.mask() as i32)
                        .unwrap()
                } else {
                    cast(LittleEndian::read_u32(&insn[self.position..]) & self.mask() as u32)
                        .unwrap()
                }
            }
            _ => unreachable!(),
        };

        value << self.shift()
    }
}

/// A CPU register in Falcon assembly.
///
/// There are 16 general-purpose register and roughly around a dozen special-purpose
/// registers. This structure holds the data necessary to parse them form instruction
/// bytes and to determine how they are used by an instruction.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Register {
    pub kind: RegisterKind,
    position: usize,
    high: bool,

    raw_value: Option<u8>,
}

impl Register {
    fn get_value(&self, byte: u8) -> u8 {
        if self.high {
            byte >> 4
        } else {
            byte & 0xF
        }
    }

    /// Reads the value that is represented by this [`Register`] from the
    /// given instruction bytes.
    ///
    /// [`Register`]: struct.Register.html
    pub fn read(&self, insn: &[u8]) -> u8 {
        if let Some(reg) = self.raw_value {
            return reg;
        }

        self.get_value(insn[self.position])
    }
}

/// A direct Falcon memory access composed of registers, immediates and magic values.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MemoryAccess {
    /// A memory access to either DMEM or IMEM that is composed of a single register.
    ///
    /// The address calculation takes the following form: `[$reg]`
    Reg(MemorySpace, Register),
    /// A memory access to either DMEM or IMEM that is composed of two registers and a
    /// value that is used for scaling.
    ///
    /// The address calculation takes the following form: `[$reg1 + $reg2 * scale]`
    RegReg(MemorySpace, Register, Register, u8),
    /// A memory access to either DMEM or IMEM that is composed of a register and an
    /// immediate value.
    ///
    /// The address calculation takes the following form: `[$reg + imm]`
    RegImm(MemorySpace, Register, Immediate<u32>),
}

impl MemoryAccess {
    pub fn position(&self) -> usize {
        match self {
            MemoryAccess::Reg(_, reg) => reg.position,
            MemoryAccess::RegReg(_, reg1, reg2, _) => max(reg1.position, reg2.position),
            MemoryAccess::RegImm(_, reg, imm) => max(reg.position, imm.position),
        }
    }

    pub fn width(&self) -> usize {
        match self {
            MemoryAccess::Reg(_, _) => 1,
            MemoryAccess::RegReg(_, _, _, _) => 1,
            MemoryAccess::RegImm(_, reg, imm) => {
                if reg.position > imm.position {
                    1
                } else {
                    imm.width
                }
            }
        }
    }
}

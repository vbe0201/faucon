//! A parser layer around actual Falcon operands, describing their position, size
//! and representation.

use std::cmp::max;

use byteorder::{ByteOrder, LittleEndian};
use num_traits::{cast, NumCast, PrimInt};

use crate::operands::{MemorySpace, RegisterKind};

macro_rules! arg {
    ($argument:expr, $variant:pat => $result:expr) => {
        if let $variant = $argument {
            Some($result)
        } else {
            None
        }
    };
}

macro_rules! immediate {
    ($v:ident, $value:tt) => {
        Argument::$v(Immediate {
            position: 0,
            width: 0,
            sign: false,
            shift: None,
            mask: None,
            raw_value: Some($value),
        })
    };
    ($v:ident, $pos:tt, $width:tt, $sign:tt, $shift:expr, $mask:expr) => {
        Argument::$v(Immediate {
            position: $pos,
            width: $width,
            sign: $sign,
            shift: $shift,
            mask: $mask,
            raw_value: None,
        })
    };
}

macro_rules! register {
    ($kind:ident, $value:tt) => {
        Argument::Register(Register {
            kind: RegisterKind::$kind,
            position: 0,
            high: false,
            raw_value: Some($value),
        })
    };
    ($kind:ident, $pos:tt, $high:tt) => {
        Argument::Register(Register {
            kind: RegisterKind::$kind,
            position: $pos,
            high: $high,
            raw_value: None,
        })
    };
}

macro_rules! memory {
    ($v:ident, $width:tt, $reg:ident) => {
        Argument::Memory(MemoryAccess::Reg(
            MemorySpace::$v,
            $width,
            arg!($reg, Argument::Register(r) => r),
        ))
    };
    ($v:ident, $width:tt, $reg1:ident, $reg2:ident, $scale:tt) => {
        Argument::Memory(MemoryAccess::RegReg(
            MemorySpace::$v,
            $width,
            arg!($reg1, Argument::Register(r) => r),
            arg!($reg2, Argument::Register(r) => r),
            $scale,
        ))
    };
    ($v:ident, $width:tt, $reg:ident, $imm:ident) => {
        Argument::Memory(MemoryAccess::RegImm(
            MemorySpace::$v,
            $width,
            arg!($reg, Argument::Register(r) => r),
            arg!($imm, Argument::U32(imm) => imm),
        ))
    };
}

/// A dummy placeholder for the statically allocated buffer of three operand
/// arguments, in case an instruction does not have three operands.
///
/// This should never be interpreted into a concrete value and shall be
/// ignored.
pub const NOP: Argument = Argument::Nop;

/// An unsigned 8-bit immediate that represents a `0` literally.
///
/// This is needed for ALU instructions where certain variants default to
/// forms with 0 instead of an actual encoded operand.
pub const NULL: Argument = immediate!(U8, 0);

/// An unsigned 8-bit immediate.
///
/// These are used for bit positions, shifts and 8-bit instructions.
pub const I8: Argument = immediate!(U8, 2, 1, false, None, None);

/// An unsigned 8-bit immediate zero-extended to 16 bits.
///
/// These are used for sethi and 16-bit instructions.
pub const I8ZX16: Argument = immediate!(U16, 2, 1, false, None, None);

/// A signed 8-bit immediate sign-extended to 16 bits.
///
/// These are used for sethi and 16-bit instructions.
pub const I8SX16: Argument = immediate!(I16, 2, 1, true, None, None);

/// An unsigned 8-bit immediate zero-extended to 32 bits.
///
/// These are used for memory addressing and most 32-bit instructions.
pub const I8ZX32: Argument = immediate!(U32, 2, 1, false, None, None);

/// A signed 32-bit immediate sign-extended to 32 bits.
///
/// These are used for memory addressing and most 32-bit instructions.
pub const I8SX32: Argument = immediate!(I32, 2, 1, true, None, None);

/// An unsigned 8-bit immediate zero-extended to 32 bits and shifted left
/// by one.
///
/// These are mainly used for memory addressing.
pub const I8ZX32S1: Argument = immediate!(U32, 2, 1, false, Some(1), None);

/// An unsigned 8-bit immediate zero-extended to 32 bits and shifted left
/// by two.
///
/// These are mainly used for memory addressing.
pub const I8ZX32S2: Argument = immediate!(U32, 2, 1, false, Some(2), None);

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
pub const I16T8: Argument = immediate!(U8, 2, 2, false, None, Some(0xFF));

/// An unsigned 16-bit immediate.
///
/// These are used by sethi and 16-bit instructions.
pub const I16: Argument = immediate!(U16, 2, 2, false, None, None);

/// An unsigned 16-bit immediate zero-extended to 32 bits.
///
/// These are used for most 32-bit instructions.
pub const I16ZX32: Argument = immediate!(U32, 2, 2, false, None, None);

/// An unsigned 16-bit immediate zero-extended to 32 bits.
///
/// These are used for Falcon v5 call instructions.
pub const I16ZX32P1: Argument = immediate!(U32, 1, 2, false, None, None);

/// A signed 16-bit immediate sign-extended to 32 bits.
///
/// These are used for most 32-bit instructions.
pub const I16SX32: Argument = immediate!(I32, 2, 2, true, None, None);

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
pub const I24ZX32: Argument = immediate!(U32, 1, 3, false, None, None);

/// An unsigned 32-bit immediate.
///
/// These are used for mov instructions.
pub const I32: Argument = immediate!(U32, 1, 4, false, None, None);

/// A Falcon general-purpose register, encoded in the low 4 bits of the second
/// instruction byte.
pub const R1: Argument = register!(Gpr, 1, false);

/// A Falcon general-purpose register, encoded in the high 4 bits of the second
/// instruction byte.
pub const R2: Argument = register!(Gpr, 1, true);

/// A Falcon general-purpose register, encoded in the high 4 bits of the third
/// instruction byte.
pub const R3: Argument = register!(Gpr, 2, true);

/// The stack pointer register.
///
/// It is used for instructions that operate on $sp by default, without
/// encoding its value in the instruction bytes.
pub const SP: Argument = register!(Spr, 4);

/// The CPU flags register.
///
/// It is used for instructions that operate on $flags by default, without
/// encoding its value in the instruction bytes.
pub const FLAGS: Argument = register!(Spr, 8);

/// A selected bit in the Falcon $flags register.
///
/// Treated as an 8-bit immediate by the hardware, used for miscellaneous
/// instructions that operate on the flag bits.
pub const FLAG: Argument = immediate!(Flag, 2, 1, false, None, Some(0x1F));

/// A software trap value.
///
/// It is used by the TRAP instruction and is encoded in the low two bits of
/// instruction byte 1, the subopcode in this case.
pub const TRAP: Argument = immediate!(U8, 1, 1, false, None, Some(0x3));

/// A Falcon special-purpose register, encoded in the high 4 bits of the second
/// instruction byte.
pub const SR1: Argument = register!(Spr, 1, true);

/// A Falcon special-purpose register, encoded in the low 4 bits of the second
/// instruction byte.
pub const SR2: Argument = register!(Spr, 1, false);

/// A memory access to an 8-bit value in Falcon DMem. The address is stored in a single
/// register.
pub const MEMR8: Argument = memory!(DMem, 8, R2);

/// A memory access to a 16-bit value in Falcon DMem. The address is stored in a single
/// register.
pub const MEMR16: Argument = memory!(DMem, 16, R2);

/// A memory access to a 32-bit value in Falcon DMem. The address is stored in a single
/// register.
pub const MEMR32: Argument = memory!(DMem, 32, R2);

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
pub const MEMRI8: Argument = memory!(DMem, 8, R2, I8ZX32);

/// A memory access to a 16-bit value in Falcon DMem. The address is composed from a
/// base address in a register and an immediate offset.
pub const MEMRI16: Argument = memory!(DMem, 16, R2, I8ZX32S1);

/// A memory access to a 32-bit value in Falcon DMem. The address is composed from a
/// base address in a register and an immediate offset.
pub const MEMRI32: Argument = memory!(DMem, 32, R2, I8ZX32S2);

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
pub const MEMSPI8: Argument = memory!(DMem, 8, SP, I8ZX32);

/// A memory access to a 16-bit value in Falcon DMem. The address is composed from a
/// base address in the `$sp` register and an immediate offset.
pub const MEMSPI16: Argument = memory!(DMem, 16, SP, I8ZX32S1);

/// A memory access to a 32-bit value in Falcon DMem. The address is composed from a
/// base address in the `$sp` register and an immediate offset.
pub const MEMSPI32: Argument = memory!(DMem, 32, SP, I8ZX32S2);

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
pub const MEMSPR8: Argument = memory!(DMem, 8, SP, R1, 1);

/// A memory access to a 16-bit value in Falcon DMem. The address is composed from a
/// base address in the `$sp` register and an offset * 2 in another register.
pub const MEMSPR16: Argument = memory!(DMem, 16, SP, R1, 2);

/// A memory access to a 32-bit value in Falcon DMem. The address is composed from a
/// base address in the `$sp` register and an offset * 4 in another register.
pub const MEMSPR32: Argument = memory!(DMem, 32, SP, R1, 4);

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
pub const MEMRR8: Argument = memory!(DMem, 8, R2, R1, 1);

/// A memory access to a 16-bit value in Falcon DMem. The address is composed from a
/// base address in a register and an offset * 2 in another register.
pub const MEMRR16: Argument = memory!(DMem, 16, R2, R1, 2);

/// A memory access to a 32-bit value in Falcon DMem. The address is composed from a
/// base address in a register and an offset * 4 in another register.
pub const MEMRR32: Argument = memory!(DMem, 32, R2, R1, 4);

/// A helper that leverages the selection of an appropriate parser for memory access
/// encodings in sized instructions to the disassembler.
pub const MEMRR: Argument = Argument::SizeConverter(|size| match size {
    0 => MEMRR8,
    1 => MEMRR16,
    2 => MEMRR32,
    _ => unreachable!(),
});

/// A memory access to a 32-bit value in Falcon IMem. The address is specified by a
/// single register.
pub const IOR: Argument = memory!(IMem, 32, R2);

/// A memory access to a 32-bit value in Falcon IMem. The address is composed from a
/// base address in a register and an offset * 4 in another register.
pub const IORR: Argument = memory!(IMem, 32, R2, R1, 4);

/// A memory access to a 32-bit value in Falcon IMem. The address is composed from a
/// base address in a register and an immediate offset.
pub const IORI: Argument = memory!(IMem, 32, R2, I8ZX32S2);

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

    /// A dummy value that is used as a hack to fulfill static allocation
    /// requirements in `faucon-asm-derive` codegen. This variant shall
    /// never be interpreted as a real value and can be safely skipped.
    Nop,
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
            _ => 0,
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
            _ => 0,
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
                    cast(insn[self.position] as i8).unwrap()
                } else {
                    cast(insn[self.position]).unwrap()
                }
            }
            2 => {
                if self.sign {
                    cast(LittleEndian::read_i16(&insn[self.position..])).unwrap()
                } else {
                    cast(LittleEndian::read_u16(&insn[self.position..])).unwrap()
                }
            }
            3 => {
                if self.sign {
                    cast(LittleEndian::read_i24(&insn[self.position..])).unwrap()
                } else {
                    cast(LittleEndian::read_u24(&insn[self.position..])).unwrap()
                }
            }
            4 => {
                if self.sign {
                    cast(LittleEndian::read_i32(&insn[self.position..])).unwrap()
                } else {
                    cast(LittleEndian::read_u32(&insn[self.position..])).unwrap()
                }
            }
            _ => unreachable!(),
        };

        (value & cast(self.mask()).unwrap()) << self.shift()
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

// FIXME: These Options technically can never be None, change the types and unwrap the arg!
//        result in the memory! macro as soon as const_panic is in stable Rust.

/// A direct Falcon memory access composed of registers, immediates and magic values.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MemoryAccess {
    /// A memory access to either DMEM or IMEM that is composed of a single register.
    ///
    /// The address calculation takes the following form: `[$reg]`
    Reg(MemorySpace, usize, Option<Register>),
    /// A memory access to either DMEM or IMEM that is composed of two registers and a
    /// value that is used for scaling.
    ///
    /// The address calculation takes the following form: `[$reg1 + $reg2 * scale]`
    RegReg(MemorySpace, usize, Option<Register>, Option<Register>, u8),
    /// A memory access to either DMEM or IMEM that is composed of a register and an
    /// immediate value.
    ///
    /// The address calculation takes the following form: `[$reg + imm]`
    RegImm(MemorySpace, usize, Option<Register>, Option<Immediate<u32>>),
}

impl MemoryAccess {
    pub fn position(&self) -> usize {
        match self {
            MemoryAccess::Reg(_, _, reg) => reg.as_ref().unwrap().position,
            MemoryAccess::RegReg(_, _, reg1, reg2, _) => max(
                reg1.as_ref().unwrap().position,
                reg2.as_ref().unwrap().position,
            ),
            MemoryAccess::RegImm(_, _, reg, imm) => max(
                reg.as_ref().unwrap().position,
                imm.as_ref().unwrap().position,
            ),
        }
    }

    pub fn width(&self) -> usize {
        match self {
            MemoryAccess::Reg(_, _, _) => 1,
            MemoryAccess::RegReg(_, _, _, _, _) => 1,
            MemoryAccess::RegImm(_, _, reg, imm) => {
                let reg = reg.as_ref().unwrap();
                let imm = imm.as_ref().unwrap();

                if reg.position > imm.position {
                    1
                } else {
                    imm.width
                }
            }
        }
    }
}

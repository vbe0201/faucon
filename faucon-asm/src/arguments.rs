//! A parser layer around actual Falcon operands, describing their position, size
//! and representation.

use byteorder::{ByteOrder, LittleEndian};
use num_traits::{cast, NumCast, PrimInt};

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

/// A dummy placeholder for the statically allocated buffer of three operand
/// arguments, in case an instruction does not have three operands.
///
/// This should never be interpreted into a concrete value and shall be
/// ignored.
pub const NOP: Argument = Argument::Nop;

/// An unsigned 8-bit immediate that represents a `0` literally.
///
/// This is needed for trap instructions where the software trap value
/// is not encoded in the instruction bytes.
pub const NULL: Argument = immediate!(U8, 0);

/// An unsigned 8-bit immediate that represents a `1` literally.
///
/// This is needed for trap instructions where the software trap value
/// is not encoded in the instruction bytes.
pub const ONE: Argument = immediate!(U8, 1);

/// An unsigned 8-bit immediate that represents a `2` literally.
///
/// This is needed for trap instructions where the software trap value
/// is not encoded in the instruction bytes.
pub const TWO: Argument = immediate!(U8, 2);

/// An unsigned 8-bit immediate that represents a `3` literally.
///
/// This is needed for trap instructions where the software trap value
/// is not encoded in the instruction bytes.
pub const THREE: Argument = immediate!(U8, 3);

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
pub const I16ZX32: Argument = immediate!(U16, 2, 2, false, None, None);

/// A signed 16-bit immediate zero-extended to 32 bits.
///
/// These are used for most 32-bit instructions.
pub const I16SX32: Argument = immediate!(I16, 2, 2, true, None, None);

/// An unsigned 24-bit immediate zero-extended to 32 bits.
///
/// These are used for absolute call/jump addresses.
pub const I24: Argument = immediate!(U24, 2, 3, false, None, None);

/// An unsigned 32-bit immediate.
///
/// These are used for mov instructions.
pub const I32: Argument = immediate!(U32, 1, 4, false, None, None);

/// Wrapper around Falcon instruction operands.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Argument {
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

    /// A dummy value that is used as a hack to fulfill static allocation
    /// requirements in `faucon-asm-derive` codegen. This variant shall
    /// never be interpreted as a real value and can be safely skipped.
    Nop,
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

use std::mem::size_of;
use std::ops::Range;
use std::ptr::copy_nonoverlapping;

use num_traits::{One, PrimInt, Zero};

// Gets the width of a value in bytes.
#[inline]
fn bytewidth(i: u32) -> usize {
    if i < 1 << 8 {
        1
    } else if i < 1 << 16 {
        2
    } else if i < 1 << 24 {
        3
    } else {
        4
    }
}

// Sign-extends `value` to the number of bytes denoted by `nbytes`.
#[inline]
fn extend_sign(value: u32, nbytes: usize) -> i32 {
    let shift = (size_of::<u32>() - nbytes) << 3;
    (value << shift) as i32 >> shift
}

// Unextends the sign of `value` from the number of bytes denoted by `nbytes`.
#[inline]
fn unextend_sign(value: i32, nbytes: usize) -> u32 {
    let shift = (size_of::<i32>() - nbytes) << 3;
    (value << shift) as u32 >> shift
}

// Sign-extends `value` to the number of bits denoted by `nbits`.
#[inline]
fn extend_bit_sign<I>(value: I, nbits: usize) -> I
where
    I: PrimInt + One + Zero,
{
    if size_of::<I>() == nbits >> 3 {
        return value;
    }

    if ((value >> (nbits - 1)) & I::one()) != I::zero() {
        value | (!I::zero()) << nbits
    } else {
        value
    }
}

// Reads `nbytes` bytes from `buf` into an unsigned integer value.
#[inline]
fn read_uint(buf: &[u8], nbytes: usize) -> u32 {
    assert!(1 <= nbytes && nbytes <= size_of::<u32>() && nbytes <= buf.len());

    let mut result: u32 = 0;
    unsafe {
        let ptr = &mut result as *mut u32 as *mut u8;
        copy_nonoverlapping(buf.as_ptr(), ptr, nbytes);
    }
    result.to_le()
}

// Reads `nbytes` bytes from `buf` into a signed integer value.
#[inline]
fn read_int(buf: &[u8], nbytes: usize) -> i32 {
    extend_sign(read_uint(buf, nbytes), nbytes)
}

// Writes `nbytes` bytes from unsigned integer value `i` to `buf`.
#[inline]
fn write_uint(buf: &mut [u8], i: u32, nbytes: usize) {
    assert!(nbytes <= 4 && nbytes <= buf.len() && bytewidth(i) <= nbytes);

    unsafe {
        let bytes = *(&i.to_le() as *const u32 as *const [u8; 4]);
        copy_nonoverlapping(bytes.as_ptr(), buf.as_mut_ptr(), nbytes);
    }
}

// Writes `nbytes` bytes from signed integer value `i` to `buf`.
#[inline]
fn write_int(buf: &mut [u8], i: i32, nbytes: usize) {
    write_uint(buf, unextend_sign(i, nbytes), nbytes)
}

// Writes `nbytes` bytes from unsigned integer value `i` to `buf` by masking it in
// without overriding existing contents.
#[inline]
fn modify_uint(buf: &mut [u8], i: u32, mask: u32, nbytes: usize) {
    assert!(nbytes <= 4 && nbytes <= buf.len() && bytewidth(i) <= nbytes);

    let new_i = (read_uint(buf, nbytes) & !mask) | i;
    write_uint(buf, new_i, nbytes);
}

// Writes `nbytes` bytes from signed integer value `i` to `buf` by masking it in
// without overriding existing contents.
#[inline]
fn modify_int(buf: &mut [u8], i: i32, mask: i32, nbytes: usize) {
    modify_uint(buf, unextend_sign(i, nbytes), mask as u32, nbytes)
}

// Aligns `value` down to the next multiple of `align`.
//
// `align` is expected to be a power of two or else this will produce incorrect results.
#[inline]
const fn align_down(value: usize, align: usize) -> usize {
    value & !(align - 1)
}

// Aligns `value` up to the next multiple of `align.`
//
// `align` is expected to be a power of two or else this will produce incorrect results.
#[inline]
const fn align_up(value: usize, align: usize) -> usize {
    align_down(value + align - 1, align)
}

// Crafts a bitmask that represents the `n` least significant bits.
#[inline]
const fn least_significant_bits(n: usize) -> usize {
    (1 << n) - 1
}

// Crafts a bitmask that extracts a given bit range from `start` (inclusive) to
// `end` (exclusive).
#[inline]
const fn bitmask(start: usize, end: usize) -> usize {
    least_significant_bits(end) & !least_significant_bits(start)
}

// Represents a specific bitfield over an integral type `I`. Used for converting
// assembly operands between machine code.
pub struct BitField<I> {
    value: Option<I>,
    range: Range<usize>,
    shift: Option<usize>,
}

impl<I> BitField<I> {
    pub const fn new(range: Range<usize>, shift: Option<usize>) -> Self {
        BitField {
            value: None,
            range,
            shift,
        }
    }

    pub const fn new_with_value(value: I, range: Range<usize>, shift: Option<usize>) -> Self {
        BitField {
            value: Some(value),
            range,
            shift,
        }
    }

    #[inline]
    pub(crate) fn byte_start(&self) -> usize {
        align_down(self.range.start, 8) >> 3
    }

    #[inline]
    pub(crate) fn is_byte_aligned(&self) -> bool {
        self.range.start & 7 == 0 && self.range.end & 7 == 0
    }

    #[inline]
    pub(crate) fn byte_width(&self) -> usize {
        align_up(self.range.end - self.range.start, 8) >> 3
    }

    #[inline]
    pub(crate) fn bitmask(&self) -> u32 {
        bitmask(self.range.start, self.range.end) as u32
    }

    #[inline]
    pub(crate) fn bitshift(&self) -> usize {
        self.range.start
    }
}

macro_rules! impl_bitfield_for {
    (unsigned $ty:ty) => {
        impl BitField<$ty> {
            #[inline]
            pub fn min(&self) -> $ty {
                0
            }

            #[inline]
            pub fn max(&self) -> $ty {
                least_significant_bits(self.range.end - self.range.start) as $ty
            }

            pub fn read(&self, buf: &[u8]) -> $ty {
                self.value.unwrap_or({
                    // Read the value that encapsulate the desired bitfield in full bytes.
                    let raw = read_uint(buf, self.byte_start() + self.byte_width());

                    // Extract the described bitfield.
                    let field = (raw & self.bitmask()) >> self.bitshift();
                    (field << self.shift.unwrap_or(0)) as $ty
                })
            }

            pub fn write(&self, buf: &mut [u8], value: $ty) {
                // Fixed values in bitfields are implied by the instruction opcode.
                // There's no need to write an actual value if that applies here.
                if self.value.is_none() {
                    let value = value as u32;

                    let raw = (value >> self.shift.unwrap_or(0)) << self.bitshift();
                    modify_uint(
                        buf,
                        raw,
                        self.bitmask(),
                        self.byte_start() + self.byte_width(),
                    );
                }
            }
        }
    };

    (signed $ty:ty) => {
        impl BitField<$ty> {
            #[inline]
            pub fn min(&self) -> $ty {
                let nbits = self.range.end - self.range.start;
                extend_bit_sign(1 << (nbits - 1), nbits)
            }

            #[inline]
            pub fn max(&self) -> $ty {
                !self.min()
            }

            pub fn read(&self, buf: &[u8]) -> $ty {
                self.value.unwrap_or({
                    // Read the value that encapsulate the desired bitfield in full bytes.
                    let raw = read_int(buf, self.byte_start() + self.byte_width());

                    // Extract the described bitfield.
                    let field = (raw & self.bitmask() as i32) >> self.bitshift();
                    (field << self.shift.unwrap_or(0)) as $ty
                })
            }

            pub fn write(&self, buf: &mut [u8], value: $ty) {
                // Fixed values in bitfields are implied by the instruction opcode.
                // There's no need to write an actual value if that applies here.
                if self.value.is_none() {
                    let value = value as i32;

                    let raw = (value >> self.shift.unwrap_or(0)) << self.bitshift();
                    modify_int(
                        buf,
                        raw,
                        self.bitmask() as i32,
                        self.byte_start() + self.byte_width(),
                    );
                }
            }
        }
    };
}

impl_bitfield_for!(signed i8);
impl_bitfield_for!(unsigned u8);
impl_bitfield_for!(signed i16);
impl_bitfield_for!(unsigned u16);
impl_bitfield_for!(signed i32);
impl_bitfield_for!(unsigned u32);

// ---- TODO: Remove this.

/// A trait that defines byte encoding for individual integer types.
///
/// This aids in providing encoding and decoding functionality between integers
/// and their underlying byte representations for processing Falcon machine code.
///
/// All encoding and decoding is done respecting little endian byte ordering.
pub trait EncodableInteger: Sized {
    /// Reads the integer type from a buffer of bytes.
    fn read_from_bytes(buf: &[u8], nbytes: usize) -> Self;

    /// Writes the integer type to a buffer of bytes.
    fn write_to_bytes(self, buf: &mut [u8], mask: Option<Self>, nbytes: usize);
}

macro_rules! impl_encodable_integer_for {
    (unsigned $ty:ty) => {
        impl EncodableInteger for $ty {
            fn read_from_bytes(buf: &[u8], nbytes: usize) -> Self {
                read_uint(buf, nbytes) as $ty
            }

            fn write_to_bytes(self, buf: &mut [u8], mask: Option<Self>, nbytes: usize) {
                match mask {
                    None => write_uint(buf, self as u32, nbytes),
                    Some(m) => modify_uint(buf, self as u32, m as u32, nbytes),
                }
            }
        }
    };
    (signed $ty:ty) => {
        impl EncodableInteger for $ty {
            fn read_from_bytes(buf: &[u8], nbytes: usize) -> Self {
                read_int(buf, nbytes) as $ty
            }

            fn write_to_bytes(self, buf: &mut [u8], mask: Option<Self>, nbytes: usize) {
                match mask {
                    None => write_int(buf, self as i32, nbytes),
                    Some(m) => modify_int(buf, self as i32, m as i32, nbytes),
                }
            }
        }
    };
}

impl_encodable_integer_for!(unsigned u8);
impl_encodable_integer_for!(signed i8);
impl_encodable_integer_for!(unsigned u16);
impl_encodable_integer_for!(signed i16);
impl_encodable_integer_for!(unsigned u32);
impl_encodable_integer_for!(signed i32);

// ----

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bitfield_helpers() {
        const BF1: BitField<u32> = BitField::new(8..16, None);
        const BF2: BitField<u32> = BitField::new(8..12, None);
        const BF3: BitField<u32> = BitField::new(17..32, None);

        assert_eq!(BF1.byte_start(), 1);
        assert!(BF1.is_byte_aligned());
        assert_eq!(BF1.byte_width(), 1);
        assert_eq!(BF1.bitmask(), 0b1111111100000000);
        assert_eq!(BF1.bitshift(), 8);

        assert_eq!(BF2.byte_start(), 1);
        assert!(!BF2.is_byte_aligned());
        assert_eq!(BF2.byte_width(), 1);
        assert_eq!(BF2.bitmask(), 0b111100000000);
        assert_eq!(BF2.bitshift(), 8);

        assert_eq!(BF3.byte_start(), 2);
        assert!(!BF3.is_byte_aligned());
        assert_eq!(BF3.byte_width(), 2);
        assert_eq!(BF3.bitmask(), 0b11111111111111100000000000000000);
        assert_eq!(BF3.bitshift(), 17);
    }

    #[test]
    fn test_bitfield_value_bounds() {
        assert_eq!(BitField::<u8>::new(0..0, None).min(), 0);
        assert_eq!(BitField::<u8>::new(0..0, None).max(), 0);
        assert_eq!(BitField::<u16>::new(0..6, None).min(), 0);
        assert_eq!(BitField::<u32>::new(0..6, None).max(), 0x3F);
        assert_eq!(BitField::<u8>::new(4..9, None).min(), 0);
        assert_eq!(BitField::<u8>::new(4..9, None).max(), 0x1F);
        assert_eq!(BitField::<u32>::new(7..32, None).min(), 0);
        assert_eq!(BitField::<u32>::new(7..32, None).max(), 0x1ffffff);
        assert_eq!(BitField::<u16>::new(8..24, None).min(), 0);
        assert_eq!(BitField::<u16>::new(8..24, None).max(), 0xFFFF);

        assert_eq!(BitField::<i32>::new(0..8, None).min(), -128);
        assert_eq!(BitField::<i8>::new(0..8, None).max(), 127);
        assert_eq!(BitField::<i16>::new(7..23, None).min(), -32768);
        assert_eq!(BitField::<i16>::new(7..23, None).max(), 32767);
        assert_eq!(BitField::<i8>::new(4..8, None).min(), -8);
        assert_eq!(BitField::<i8>::new(4..8, None).max(), 7);
        assert_eq!(BitField::<i32>::new(2..8, None).min(), -32);
        assert_eq!(BitField::<i16>::new(2..8, None).max(), 31);
    }

    #[test]
    fn test_unsigned_bitfield_extraction() {
        const BF1: BitField<u16> = BitField::new(8..24, None);
        const BF2: BitField<u32> = BitField::new(9..24, None);
        const BF3: BitField<u32> = BitField::new(9..23, None);

        let test_buf: &[u8] = &[0xDE, 0xAD, 0xBE, 0xEF];
        assert_eq!(BF1.read(test_buf), 0xBEAD);
        assert_eq!(BF2.read(test_buf), 0x5F56);
        assert_eq!(BF3.read(test_buf), 0x1F56);
    }

    #[test]
    fn test_unsigned_bitfield_insertion() {
        const BF1: BitField<u8> = BitField::new(0..2, None);
        const BF2: BitField<u8> = BitField::new(2..6, None);
        const BF3: BitField<u32> = BitField::new(6..32, None);

        let test_buf: &mut [u8] = &mut [0, 0, 0, 0];

        BF1.write(test_buf, 0x2);
        assert_eq!(test_buf, &[0x2, 0, 0, 0]);

        BF2.write(test_buf, 0xF);
        assert_eq!(test_buf, &[0x3E, 0, 0, 0]);

        BF3.write(test_buf, 0xFFFFFFC0);
        assert_eq!(test_buf, &[0x3E, 0xF0, 0xFF, 0xFF]);
    }

    // TODO: Signed bitfield insertion/extraction tests.
}

//! Miscellaneous utilities for manipulating binary data.

/// Loads at most 8 input bytes to an integer in LE ordering.
#[inline]
pub fn load_le(input: &[u8]) -> u64 {
    let mut res = [0; 8];
    let limit = input.len().min(8);
    res[..limit].copy_from_slice(&input[..limit]);
    u64::from_le_bytes(res)
}

/// Aligns `value` up to the next multiple of `alignment`.
#[inline]
pub const fn align_up(value: u64, alignment: u32) -> u64 {
    align_down(value + alignment as u64 - 1, alignment)
}

/// Aligns `value` down to the next multiple of `alignment`.
#[inline]
pub const fn align_down(value: u64, alignment: u32) -> u64 {
    debug_assert!(alignment.is_power_of_two());
    value & !(alignment as u64 - 1)
}

/// Crafts a bit mask with the lowest `n` bits set to 1.
#[inline]
pub const fn low_bit_mask(n: u32) -> u64 {
    assert!(n < u64::BITS);
    (1 << n) - 1
}

/// Crafts a bit mask where bit range `[start, end)` is set to 1.
#[inline]
pub const fn mask_bit_range(start: u32, end: u32) -> u64 {
    low_bit_mask(end) & !low_bit_mask(start)
}

/// Sign-extends the low `nbits` bits of a value.
#[inline]
pub const fn extend_sign(value: u64, nbits: u32) -> i64 {
    let shift = u64::BITS - nbits;
    (value << shift) as i64 >> shift
}

/// Helper for encoding and decoding data at bit-level granularity.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BitField {
    value: u8,
    start: u8,
    end: u8,
    shift: u8,
}

impl BitField {
    /// Creates a bit field with a fixed value assigned.
    #[inline]
    pub const fn new_with_value(value: u8) -> Self {
        Self {
            value,
            start: 0,
            end: 0,
            shift: 0,
        }
    }

    /// Creates a bit field spanning bit range `[start, end)` with a
    /// bitwise shift applied to the value.
    #[inline]
    pub const fn new_with_shift(start: u8, end: u8, shift: u8) -> Self {
        debug_assert!(start <= end);
        Self {
            value: 0,
            start,
            end,
            shift,
        }
    }

    /// Creates a bit field spanning bit range `[start, end)`.
    #[inline]
    pub const fn new(start: u8, end: u8) -> Self {
        Self::new_with_shift(start, end, 0)
    }

    #[inline]
    const fn is_fixed(&self) -> bool {
        self.start == self.end
    }

    /// Gets the start bit index (inclusive) of the field.
    #[inline]
    pub const fn start(&self) -> u8 {
        self.start
    }

    /// Gets the end bit index (exclusive) of the field.
    #[inline]
    pub const fn end(&self) -> u8 {
        self.end
    }

    /// Gets the width of the field in bits.
    #[inline]
    pub const fn bit_width(&self) -> u32 {
        (self.end - self.start) as u32
    }

    /// Gets the byte offset where the field starts.
    #[inline]
    pub const fn byte_start(&self) -> u8 {
        let byte_aligned_start = align_down(self.start as u64, 8);
        byte_aligned_start as u8 / 8
    }

    /// Gets the byte offset where the field ends.
    #[inline]
    pub const fn byte_end(&self) -> u8 {
        let byte_aligned_end = align_up(self.end as u64, 8);
        byte_aligned_end as u8 / 8
    }

    /// Decodes the field from a given target datum and returns it.
    #[inline]
    pub const fn decode(&self, target: u64) -> u64 {
        if self.is_fixed() {
            self.value as u64
        } else {
            let mask = mask_bit_range(self.start as _, self.end as _);
            let field = (target & mask) >> self.start;
            field << self.shift
        }
    }

    /// Encodes the given field value into the target datum and
    /// returns the updated datum.
    #[inline]
    pub const fn encode(&self, target: u64, value: u64) -> u64 {
        if self.is_fixed() {
            target
        } else {
            let mask = mask_bit_range(self.start as _, self.end as _);
            let value = (value >> self.shift) << self.start;
            (target & !mask) | (value & mask)
        }
    }
}

use std::mem::size_of;
use std::ptr::copy_nonoverlapping;

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

#[inline]
fn extend_sign(value: u32, nbytes: usize) -> i32 {
    let shift = (size_of::<u32>() - nbytes) << 3;
    (value << shift) as i32 >> shift
}

#[inline]
fn unextend_sign(value: i32, nbytes: usize) -> u32 {
    let shift = (size_of::<i32>() - nbytes) << 3;
    (value << shift) as u32 >> shift
}

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

#[inline]
fn read_int(buf: &[u8], nbytes: usize) -> i32 {
    extend_sign(read_uint(buf, nbytes), nbytes)
}

#[inline]
fn write_uint(buf: &mut [u8], i: u32, mask: u32, nbytes: usize) {
    assert!(nbytes <= 4 && nbytes <= buf.len() && bytewidth(i) <= nbytes);

    let bytes = i.to_le_bytes();
    for i in 0..nbytes {
        buf[i] = (buf[i] & !(mask >> (i << 3)) as u8) | bytes[i];
    }
}

#[inline]
fn write_int(buf: &mut [u8], i: i32, mask: i32, nbytes: usize) {
    write_uint(buf, unextend_sign(i, nbytes), mask as u32, nbytes)
}

/// A trait that defines byte encoding for individual integer types.
///
/// This aids in providing encoding and decoding functionality between integers
/// and their underlying byte representations for processing Falcon machine code.
///
/// All encoding and decoding is done respecting little endian byte ordering.
pub trait ByteEncoding: Sized {
    /// Reads the integer type from a buffer of bytes.
    fn read_from_bytes(buf: &[u8], nbytes: usize) -> Self;

    /// Writes the integer type to a buffer of bytes.
    fn write_to_bytes(self, buf: &mut [u8], mask: Self, nbytes: usize);
}

macro_rules! impl_byteencoding_for {
    (unsigned $ty:ty) => {
        impl ByteEncoding for $ty {
            fn read_from_bytes(buf: &[u8], nbytes: usize) -> Self {
                read_uint(buf, nbytes) as $ty
            }

            fn write_to_bytes(self, buf: &mut [u8], mask: Self, nbytes: usize) {
                write_uint(buf, self as u32, mask as u32, nbytes)
            }
        }
    };
    (signed $ty:ty) => {
        impl ByteEncoding for $ty {
            fn read_from_bytes(buf: &[u8], nbytes: usize) -> Self {
                read_int(buf, nbytes) as $ty
            }

            fn write_to_bytes(self, buf: &mut [u8], mask: Self, nbytes: usize) {
                write_int(buf, self as i32, mask as i32, nbytes)
            }
        }
    };
}

impl_byteencoding_for!(unsigned u8);
impl_byteencoding_for!(signed i8);
impl_byteencoding_for!(unsigned u16);
impl_byteencoding_for!(signed i16);
impl_byteencoding_for!(unsigned u32);
impl_byteencoding_for!(signed i32);

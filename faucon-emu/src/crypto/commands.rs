//! The AES crypto commands that can be performed by the Secure Co-Processor.

// This file provides an entire 128-bit AES-ECB implementation from scratch due to very
// special demands of the Falcon cryptosystem that cannot be satisfied by existing
// libraries. Please do not use any of the code here as a reference for your own projects
// (unless you're writing a Falcon emulator too), there are better ways to implement AES
// but that's just how the Falcon works. Refer to RustCrypto for proper AES crates.

use rand::Rng;

/// Generates a block of random data from a strong RNG source.
pub fn crnd() -> [u8; 0x10] {
    rand::thread_rng().gen()
}

/// Overwrites the contents of the `a` block by XORing the contents of `b` into it.
pub fn cxor(a: &mut [u8; 0x10], b: &[u8; 0x10]) {
    for (x, y) in a.iter_mut().zip(b.iter().cycle()) {
        *x ^= y;
    }
}

/// Overwrites the contents of the `a` block by ANDing the contents of `b` into it.
pub fn cand(a: &mut [u8; 0x10], b: &[u8; 0x10]) {
    for (x, y) in a.iter_mut().zip(b.iter().cycle()) {
        *x &= y;
    }
}

/// Creates a new block and fills it with the endian-swapped/reversed input block.
pub fn crev(a: &[u8; 0x10]) -> [u8; 0x10] {
    let mut b = a.clone();
    b.reverse();

    b
}

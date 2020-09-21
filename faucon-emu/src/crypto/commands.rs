//! The AES crypto commands that can be performed by the Secure Co-Processor.

// This file provides an entire 128-bit AES-ECB implementation from scratch due to very
// special demands of the Falcon cryptosystem that cannot be satisfied by existing
// libraries. Please do not use any of the code here as a reference for your own projects
// (unless you're writing a Falcon emulator too), there are better ways to implement AES
// but that's just how the Falcon works. Refer to RustCrypto for proper AES crates.

use rand::Rng;

/// A block to be processed by the AES algorithm.
pub type Block = [u8; 0x10];

/// The AES Cipher Key to be used for cryptographic operations.
pub type Key = [u8; 0x10];

/// A Round Key for a specific round that can be obtained by expanding the Cipher Key.
pub type RoundKey = [u8; 0x10];

/// Generates a block of random data from a strong RNG source.
pub fn crnd() -> Block {
    rand::thread_rng().gen()
}

/// Overwrites the contents of the `a` block by XORing the contents of `b` into it.
pub fn cxor(a: &mut Block, b: &Block) {
    for (x, y) in a.iter_mut().zip(b.iter().cycle()) {
        *x ^= y;
    }
}

/// Overwrites the contents of the `a` block by ANDing the contents of `b` into it.
pub fn cand(a: &mut Block, b: &Block) {
    for (x, y) in a.iter_mut().zip(b.iter().cycle()) {
        *x &= y;
    }
}

/// Creates a new block and fills it with the endian-swapped/reversed input block.
pub fn crev(a: &Block) -> Block {
    let mut b = a.clone();
    b.reverse();

    b
}

/// Performs a multiplication of `a` interpreted as a `u128` by 2 in the [GF(2^8) finite field]
/// defined by the polynomial `x^8 + x^4 + x^3 + x + 1 = 0`.
///
/// [GF(2^8) finite field]: https://en.wikipedia.org/wiki/Finite_field_arithmetic
pub fn cgfmul(a: &Block) -> Block {
    let block = u128::from_le_bytes(*a);
    let mut result = block << 1;
    if block & 0x80000000000000000000000000000000 != 0 {
        result ^= 0x1B;
    }

    result.to_le_bytes()
}

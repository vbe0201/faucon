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

/// The S-box transformation table.
const SBOX: [u8; 256] = [
    0x63, 0x7C, 0x77, 0x7B, 0xF2, 0x6B, 0x6F, 0xC5, 0x30, 0x01, 0x67, 0x2B, 0xFE, 0xD7, 0xAB, 0x76,
    0xCA, 0x82, 0xC9, 0x7D, 0xFA, 0x59, 0x47, 0xF0, 0xAD, 0xD4, 0xA2, 0xAF, 0x9C, 0xA4, 0x72, 0xC0,
    0xB7, 0xFD, 0x93, 0x26, 0x36, 0x3F, 0xF7, 0xCC, 0x34, 0xA5, 0xE5, 0xF1, 0x71, 0xD8, 0x31, 0x15,
    0x04, 0xC7, 0x23, 0xC3, 0x18, 0x96, 0x05, 0x9A, 0x07, 0x12, 0x80, 0xE2, 0xEB, 0x27, 0xB2, 0x75,
    0x09, 0x83, 0x2C, 0x1A, 0x1B, 0x6E, 0x5A, 0xA0, 0x52, 0x3B, 0xD6, 0xB3, 0x29, 0xE3, 0x2F, 0x84,
    0x53, 0xD1, 0x00, 0xED, 0x20, 0xFC, 0xB1, 0x5B, 0x6A, 0xCB, 0xBE, 0x39, 0x4A, 0x4C, 0x58, 0xCF,
    0xD0, 0xEF, 0xAA, 0xFB, 0x43, 0x4D, 0x33, 0x85, 0x45, 0xF9, 0x02, 0x7F, 0x50, 0x3C, 0x9F, 0xA8,
    0x51, 0xA3, 0x40, 0x8F, 0x92, 0x9D, 0x38, 0xF5, 0xBC, 0xB6, 0xDA, 0x21, 0x10, 0xFF, 0xF3, 0xD2,
    0xCD, 0x0C, 0x13, 0xEC, 0x5F, 0x97, 0x44, 0x17, 0xC4, 0xA7, 0x7E, 0x3D, 0x64, 0x5D, 0x19, 0x73,
    0x60, 0x81, 0x4F, 0xDC, 0x22, 0x2A, 0x90, 0x88, 0x46, 0xEE, 0xB8, 0x14, 0xDE, 0x5E, 0x0B, 0xDB,
    0xE0, 0x32, 0x3A, 0x0A, 0x49, 0x06, 0x24, 0x5C, 0xC2, 0xD3, 0xAC, 0x62, 0x91, 0x95, 0xE4, 0x79,
    0xE7, 0xC8, 0x37, 0x6D, 0x8D, 0xD5, 0x4E, 0xA9, 0x6C, 0x56, 0xF4, 0xEA, 0x65, 0x7A, 0xAE, 0x08,
    0xBA, 0x78, 0x25, 0x2E, 0x1C, 0xA6, 0xB4, 0xC6, 0xE8, 0xDD, 0x74, 0x1F, 0x4B, 0xBD, 0x8B, 0x8A,
    0x70, 0x3E, 0xB5, 0x66, 0x48, 0x03, 0xF6, 0x0E, 0x61, 0x35, 0x57, 0xB9, 0x86, 0xC1, 0x1D, 0x9E,
    0xE1, 0xF8, 0x98, 0x11, 0x69, 0xD9, 0x8E, 0x94, 0x9B, 0x1E, 0x87, 0xE9, 0xCE, 0x55, 0x28, 0xDF,
    0x8C, 0xA1, 0x89, 0x0D, 0xBF, 0xE6, 0x42, 0x68, 0x41, 0x99, 0x2D, 0x0F, 0xB0, 0x54, 0xBB, 0x16,
];

fn key_schedule_round(key: &RoundKey, rcon: u8) -> RoundKey {
    let mut round_key = [0; 0x10];
    round_key.copy_from_slice(key);

    // Rotate the previous word and apply S-box. Also XOR RCON for the first byte.
    round_key[0] ^= SBOX[round_key[13] as usize] ^ rcon;
    round_key[1] ^= SBOX[round_key[14] as usize];
    round_key[2] ^= SBOX[round_key[15] as usize];
    round_key[3] ^= SBOX[round_key[12] as usize];

    for round in 1..4 {
        // XOR in the previous word.
        for i in 0..4 {
            round_key[4 * round + i] ^= round_key[4 * (round - 1) + i];
        }
    }

    round_key
}

fn invert_key_schedule_round(key: &RoundKey, rcon: u8) -> RoundKey {
    let mut round_key = [0; 0x10];
    round_key.copy_from_slice(key);

    for round in (1..4).rev() {
        // XOR in the previous word.
        for i in 0..4 {
            round_key[4 * round + i] ^= round_key[4 * (round - 1) + i];
        }
    }

    // Rotate the previous word and apply S-box. Also XOR RCON for the first byte.
    round_key[0] ^= SBOX[round_key[13] as usize] ^ rcon;
    round_key[1] ^= SBOX[round_key[14] as usize];
    round_key[2] ^= SBOX[round_key[15] as usize];
    round_key[3] ^= SBOX[round_key[12] as usize];

    round_key
}

fn gfmul2(a: u8) -> u8 {
    if a & 0x80 != 0 {
        (a << 1) ^ 0x1B
    } else {
        a << 1
    }
}

fn gfdiv2(a: u8) -> u8 {
    if a & 1 != 0 {
        (a >> 1) ^ 141
    } else {
        a >> 1
    }
}

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

/// Expands the given Cipher Key into the last Round Key, needed for decryption.
pub fn ckexp(key: &Key) -> RoundKey {
    let mut rcon = 1;

    // The first Round Key is the 16 bytes of the Cipher Key.
    let mut round_key = [0; 0x10];
    round_key.copy_from_slice(key);

    for _ in 0..10 {
        // Go through the Key Schedule to get the next key.
        round_key = key_schedule_round(&round_key, rcon);

        rcon = gfmul2(rcon);
    }

    round_key
}

/// Reverts the key schedule from the given last Round Key into the Cipher Key.
pub fn ckrexp(round_key: &RoundKey) -> Key {
    let mut rcon = 0x36;

    // The first (last) Round Key is the given 16 bytes key.
    let mut key = [0; 0x10];
    key.copy_from_slice(round_key);

    for _ in (0..10).rev() {
        // Go through the Key Schedule to get the previous key.
        key = invert_key_schedule_round(&key, rcon);

        rcon = gfdiv2(rcon);
    }

    key
}

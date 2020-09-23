//! Implementation of the Falcon Security Co-Processor and related crypto functionality.

mod aes;

use rand::Rng;

/// Generates a block of random data using a strong RNG source.
pub fn rnd() -> aes::Block {
    rand::thread_rng().gen()
}

/// Overwrites the contents of the `a` block by XORing the contents of `b` into it.
pub fn xor(a: &mut aes::Block, b: &aes::Block) {
    for (x, y) in a.iter_mut().zip(b.iter().cycle()) {
        *x ^= y;
    }
}

/// Overwrites the contents of the `a` block by ANDing the contents of `b` into it.
pub fn and(a: &mut aes::Block, b: &aes::Block) {
    for (x, y) in a.iter_mut().zip(b.iter().cycle()) {
        *x &= y;
    }
}

/// Creates and returns a new block filled it with the byte-reversed input block.
pub fn rev(a: &aes::Block) -> aes::Block {
    let mut b = a.clone();
    b.reverse();

    b
}

/// Performs a multiplication of `a` interpreted as a `u128` by 2 in the [GF(2^8) finite field]
/// defined by the polynomial `x^8 + x^4 + x^3 + x + 1 = 0`.
///
/// [GF(2^8) finite field]: https://en.wikipedia.org/wiki/Finite_field_arithmetic
pub fn gfmul(a: &aes::Block) -> aes::Block {
    let block = u128::from_le_bytes(*a);
    let mut result = block << 1;
    if block & 0x80000000000000000000000000000000 != 0 {
        result ^= 0x1B; // TODO: Was it 0x1B or 0x87?
    }

    result.to_le_bytes()
}

/// Expands the given Cipher Key into the last Round Key, needed for decryption.
pub fn kexp(key: &aes::Key) -> aes::RoundKey {
    let mut round_key = key.clone();
    let mut rcon = 1;

    for _ in 0..10 {
        // Go through the Key Schedule to obtain the next Round Key.
        aes::key_schedule_round(&mut round_key, rcon);

        rcon = aes::gfmul2(rcon);
    }

    round_key
}

pub fn krexp(round_key: &aes::RoundKey) -> aes::Key {
    let mut key = round_key.clone();
    let mut rcon = 0x36;

    for _ in 0..10 {
        // Go through the Key Schedule to obtain the previous Round Key.
        aes::invert_key_schedule_round(&mut key, rcon);

        rcon = aes::gfdiv2(rcon);
    }

    key
}

/// Encrypts a message with 128-bit AES-ECB using the given key.
pub fn enc(key: &aes::Key, message: &aes::Block) -> aes::Block {
    aes::encrypt(key, message)
}

/// Decrypts a message with 128-bit AES-ECB using the given key.
pub fn dec(round_key: &aes::RoundKey, message: &aes::Block) -> aes::Block {
    aes::decrypt(round_key, message)
}

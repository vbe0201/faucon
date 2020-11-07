//! Implementation of the Rijndael block cipher for use in SCP crypto commands.

// This file provides an entire 128-bit AES implementation from scratch due to very
// special demands of the Falcon cryptosystem that cannot be satisfied by existing
// libraries. Please do not use any of the code as a reference for your own projects
// (unless you are writing a Falcon emulator too), there are better ways to implement
// AES but this is just how the Falcon works. Refer to RustCrypto for proper crates.

/// A block to be processed by the AES algorithm.
pub type Block = [u8; 16];

/// The AES Cipher Key to be used for cryptographic operations.
pub type Key = [u8; 16];

/// A Round Key for a specific round that can be obtained by expanding the Cipher Key.
pub type RoundKey = [u8; 16];

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

/// The Inverse S-box transformation table.
const RSBOX: [u8; 256] = [
    0x52, 0x09, 0x6A, 0xD5, 0x30, 0x36, 0xA5, 0x38, 0xBF, 0x40, 0xA3, 0x9E, 0x81, 0xF3, 0xD7, 0xFB,
    0x7C, 0xE3, 0x39, 0x82, 0x9B, 0x2F, 0xFF, 0x87, 0x34, 0x8E, 0x43, 0x44, 0xC4, 0xDE, 0xE9, 0xCB,
    0x54, 0x7B, 0x94, 0x32, 0xA6, 0xC2, 0x23, 0x3D, 0xEE, 0x4C, 0x95, 0x0B, 0x42, 0xFA, 0xC3, 0x4E,
    0x08, 0x2E, 0xA1, 0x66, 0x28, 0xD9, 0x24, 0xB2, 0x76, 0x5B, 0xA2, 0x49, 0x6D, 0x8B, 0xD1, 0x25,
    0x72, 0xF8, 0xF6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xD4, 0xA4, 0x5C, 0xCC, 0x5D, 0x65, 0xB6, 0x92,
    0x6C, 0x70, 0x48, 0x50, 0xFD, 0xED, 0xB9, 0xDA, 0x5E, 0x15, 0x46, 0x57, 0xA7, 0x8D, 0x9D, 0x84,
    0x90, 0xD8, 0xAB, 0x00, 0x8C, 0xBC, 0xD3, 0x0A, 0xF7, 0xE4, 0x58, 0x05, 0xB8, 0xB3, 0x45, 0x06,
    0xD0, 0x2C, 0x1E, 0x8F, 0xCA, 0x3F, 0x0F, 0x02, 0xC1, 0xAF, 0xBD, 0x03, 0x01, 0x13, 0x8A, 0x6B,
    0x3A, 0x91, 0x11, 0x41, 0x4F, 0x67, 0xDC, 0xEA, 0x97, 0xF2, 0xCF, 0xCE, 0xF0, 0xB4, 0xE6, 0x73,
    0x96, 0xAC, 0x74, 0x22, 0xE7, 0xAD, 0x35, 0x85, 0xE2, 0xF9, 0x37, 0xE8, 0x1C, 0x75, 0xDF, 0x6E,
    0x47, 0xF1, 0x1A, 0x71, 0x1D, 0x29, 0xC5, 0x89, 0x6F, 0xB7, 0x62, 0x0E, 0xAA, 0x18, 0xBE, 0x1B,
    0xFC, 0x56, 0x3E, 0x4B, 0xC6, 0xD2, 0x79, 0x20, 0x9A, 0xDB, 0xC0, 0xFE, 0x78, 0xCD, 0x5A, 0xF4,
    0x1F, 0xDD, 0xA8, 0x33, 0x88, 0x07, 0xC7, 0x31, 0xB1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xEC, 0x5F,
    0x60, 0x51, 0x7F, 0xA9, 0x19, 0xB5, 0x4A, 0x0D, 0x2D, 0xE5, 0x7A, 0x9F, 0x93, 0xC9, 0x9C, 0xEF,
    0xA0, 0xE0, 0x3B, 0x4D, 0xAE, 0x2A, 0xF5, 0xB0, 0xC8, 0xEB, 0xBB, 0x3C, 0x83, 0x53, 0x99, 0x61,
    0x17, 0x2B, 0x04, 0x7E, 0xBA, 0x77, 0xD6, 0x26, 0xE1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0C, 0x7D,
];

/// Multiplies `a` by `2` in the [GF(2^8) finite field] defined by the polynomial
/// `x^8 + x^4 + x^3 + x + 1 = 0`.
///
/// [GF(2^8) finite field]: https://en.wikipedia.org/wiki/Finite_field_arithmetic
pub fn gfmul2(a: u8) -> u8 {
    if a & 0x80 != 0 {
        (a << 1) ^ 0x1B
    } else {
        a << 1
    }
}

#[inline(always)]
fn gfmul3(a: u8) -> u8 {
    gfmul2(a) ^ a
}

#[inline(always)]
fn gfmul4(a: u8) -> u8 {
    gfmul2(gfmul2(a))
}

#[inline(always)]
fn gfmul8(a: u8) -> u8 {
    gfmul2(gfmul4(a))
}

#[inline(always)]
fn gfmul9(a: u8) -> u8 {
    gfmul8(a) ^ a
}

#[inline(always)]
fn gfmul11(a: u8) -> u8 {
    gfmul8(a) ^ gfmul2(a) ^ a
}

#[inline(always)]
fn gfmul13(a: u8) -> u8 {
    gfmul8(a) ^ gfmul4(a) ^ a
}

#[inline(always)]
fn gfmul14(a: u8) -> u8 {
    gfmul8(a) ^ gfmul4(a) ^ gfmul2(a)
}

/// Divides `a` by `2` in the [GF(2^8) finite field] defined by the polynomial
/// `x^8 + x^4 + x^3 + x + 1 = 0`.
///
/// [GF(2^8) finite field]: https://en.wikipedia.org/wiki/Finite_field_arithmetic
pub fn gfdiv2(a: u8) -> u8 {
    if a & 1 != 0 {
        (a >> 1) ^ 141
    } else {
        a >> 1
    }
}

/// Goes through the AES Key Schedule for one round to derive the next Round Key.
///
/// The function must be called with the last obtained Round Key and an appropriate
/// round constant.
pub fn key_schedule_round(round_key: &mut RoundKey, rcon: u8) {
    // Rotate the previous word and substitute with S-box. Also XOR RCON for the first byte.
    round_key[0] ^= SBOX[round_key[13] as usize] ^ rcon;
    round_key[1] ^= SBOX[round_key[14] as usize];
    round_key[2] ^= SBOX[round_key[15] as usize];
    round_key[3] ^= SBOX[round_key[12] as usize];

    for round in 1..4 {
        // XOR in the previous word.
        round_key[4 * round + 0] ^= round_key[4 * (round - 1) + 0];
        round_key[4 * round + 1] ^= round_key[4 * (round - 1) + 1];
        round_key[4 * round + 2] ^= round_key[4 * (round - 1) + 2];
        round_key[4 * round + 3] ^= round_key[4 * (round - 1) + 3];
    }
}

pub fn invert_key_schedule_round(round_key: &mut RoundKey, rcon: u8) {
    for round in (1..4).rev() {
        // XOR in the previous word.
        round_key[4 * round + 0] ^= round_key[4 * (round - 1) + 0];
        round_key[4 * round + 1] ^= round_key[4 * (round - 1) + 1];
        round_key[4 * round + 2] ^= round_key[4 * (round - 1) + 2];
        round_key[4 * round + 3] ^= round_key[4 * (round - 1) + 3];
    }

    // Rotate the previous word and substitute with S-box. Also XOR RCON for the first byte.
    round_key[0] ^= SBOX[round_key[13] as usize] ^ rcon;
    round_key[1] ^= SBOX[round_key[14] as usize];
    round_key[2] ^= SBOX[round_key[15] as usize];
    round_key[3] ^= SBOX[round_key[12] as usize];
}

#[inline(always)]
fn add_round_key(state: &mut Block, key: &RoundKey) {
    for i in 0..4 {
        state[i + 0 * 4] ^= key[4 * i + 0];
        state[i + 1 * 4] ^= key[4 * i + 1];
        state[i + 2 * 4] ^= key[4 * i + 2];
        state[i + 3 * 4] ^= key[4 * i + 3];
    }
}

#[inline(always)]
fn sub_bytes(state: &mut Block) {
    for b in state.iter_mut() {
        *b = SBOX[*b as usize];
    }
}

#[inline(always)]
fn invert_sub_bytes(state: &mut Block) {
    for b in state.iter_mut() {
        *b = RSBOX[*b as usize];
    }
}

#[inline(always)]
fn shift_rows(state: &mut Block) {
    let temp = state.clone();

    // Shift the second row.
    state[4] = temp[5];
    state[5] = temp[6];
    state[6] = temp[7];
    state[7] = temp[4];

    // Shift the third row.
    state[8] = temp[10];
    state[9] = temp[11];
    state[10] = temp[8];
    state[11] = temp[9];

    // Shift the fourth row.
    state[12] = temp[15];
    state[13] = temp[12];
    state[14] = temp[13];
    state[15] = temp[14];
}

#[inline(always)]
fn invert_shift_rows(state: &mut Block) {
    let temp = state.clone();

    // Shift back the second row.
    state[4] = temp[7];
    state[5] = temp[4];
    state[6] = temp[5];
    state[7] = temp[6];

    // Shift back the third row.
    state[8] = temp[10];
    state[9] = temp[11];
    state[10] = temp[8];
    state[11] = temp[9];

    // Shift back the fourth row.
    state[12] = temp[13];
    state[13] = temp[14];
    state[14] = temp[15];
    state[15] = temp[12];
}

#[inline(always)]
fn mix_columns(state: &mut Block) {
    for depth in 0..4 {
        let s0 = state[depth + 0 * 4];
        let s1 = state[depth + 1 * 4];
        let s2 = state[depth + 2 * 4];
        let s3 = state[depth + 3 * 4];

        // [02 03 01 01]   [s0  s4  s8   s12]
        // [01 02 03 01]   [s1  s5  s9   s13]
        // [01 01 02 03]   [s2  s6  s10  s14]
        // [03 01 01 02]   [s3  s7  s11  s15]
        state[depth + 0 * 4] = gfmul2(s0) ^ gfmul3(s1) ^ s2 ^ s3;
        state[depth + 1 * 4] = s0 ^ gfmul2(s1) ^ gfmul3(s2) ^ s3;
        state[depth + 2 * 4] = s0 ^ s1 ^ gfmul2(s2) ^ gfmul3(s3);
        state[depth + 3 * 4] = gfmul3(s0) ^ s1 ^ s2 ^ gfmul2(s3);
    }
}

#[inline(always)]
fn invert_mix_columns(state: &mut Block) {
    for depth in 0..4 {
        let s0 = state[depth + 0 * 4];
        let s1 = state[depth + 1 * 4];
        let s2 = state[depth + 2 * 4];
        let s3 = state[depth + 3 * 4];

        // [0e 0b 0d 09]   [s0  s4  s8  s12]
        // [09 0e 0b 0d]   [s1  s5  s9  s13]
        // [0d 09 0e 0b]   [s2  s6  s10 s14]
        // [0b 0d 09 0e]   [s3  s7  s11 s15]
        state[depth + 0 * 4] = gfmul14(s0) ^ gfmul11(s1) ^ gfmul13(s2) ^ gfmul9(s3);
        state[depth + 1 * 4] = gfmul9(s0) ^ gfmul14(s1) ^ gfmul11(s2) ^ gfmul13(s3);
        state[depth + 2 * 4] = gfmul13(s0) ^ gfmul9(s1) ^ gfmul14(s2) ^ gfmul11(s3);
        state[depth + 3 * 4] = gfmul11(s0) ^ gfmul13(s1) ^ gfmul9(s2) ^ gfmul14(s3);
    }
}

/// Encrypts a block of data with AES-ECB using the supplied key.
pub fn encrypt(key: &Key, block: &Block) -> Block {
    let mut round_key = key.clone();
    let mut rcon = 1;

    // Initialize the State with the columns of the block.
    let mut state = [0; 16];
    for i in 0..4 {
        state[4 * i + 0] = block[i + 0 * 4];
        state[4 * i + 1] = block[i + 1 * 4];
        state[4 * i + 2] = block[i + 2 * 4];
        state[4 * i + 3] = block[i + 3 * 4];
    }

    // Add the Round Key to the State in the initial round.
    add_round_key(&mut state, &round_key);

    // Process the main rounds.
    for _ in 1..10 {
        sub_bytes(&mut state);
        shift_rows(&mut state);
        mix_columns(&mut state);
        key_schedule_round(&mut round_key, rcon);
        add_round_key(&mut state, &round_key);

        rcon = gfmul2(rcon);
    }

    // Process the final round.
    sub_bytes(&mut state);
    shift_rows(&mut state);
    key_schedule_round(&mut round_key, rcon);
    add_round_key(&mut state, &round_key);

    // Copy the ciphertext back from the State.
    let mut ciphertext = [0; 16];
    for i in 0..4 {
        ciphertext[i + 0 * 4] = state[4 * i + 0];
        ciphertext[i + 1 * 4] = state[4 * i + 1];
        ciphertext[i + 2 * 4] = state[4 * i + 2];
        ciphertext[i + 3 * 4] = state[4 * i + 3];
    }

    ciphertext
}

/// Decrypts a block of data with AES-ECB using the supplied key.
///
/// NOTE: Unlike [`encrypt`], this function does not take the Cipher Key as an argument
/// because decryption really starts with the last Round Key and walks up the Key Schedule
/// from there to the top (i.e. the Cipher Key). The user is expected to supply a correctly
/// expanded Round Key of the Cipher Key that was used for encryption.
///
/// [`encrypt`]: fn.encrypt.html
pub fn decrypt(last_round_key: &RoundKey, block: &Block) -> Block {
    let mut round_key = last_round_key.clone();
    let mut rcon = 0x36;

    // Initialize the State with the columns of the block.
    let mut state = [0; 16];
    for i in 0..4 {
        state[i + 0 * 4] = block[4 * i + 0];
        state[i + 1 * 4] = block[4 * i + 1];
        state[i + 2 * 4] = block[4 * i + 2];
        state[i + 3 * 4] = block[4 * i + 3];
    }

    // Process the first round.
    add_round_key(&mut state, &round_key);
    invert_key_schedule_round(&mut round_key, rcon);
    invert_shift_rows(&mut state);
    invert_sub_bytes(&mut state);

    // Process the main rounds.
    for _ in 1..10 {
        rcon = gfdiv2(rcon);

        add_round_key(&mut state, &round_key);
        invert_key_schedule_round(&mut round_key, rcon);
        invert_mix_columns(&mut state);
        invert_shift_rows(&mut state);
        invert_sub_bytes(&mut state);
    }

    // Add the Cipher Key to the state in the last round.
    add_round_key(&mut state, &round_key);

    // Copy the plaintext back from the State.
    let mut plaintext = [0; 16];
    for i in 0..4 {
        plaintext[4 * i + 0] = state[i + 0 * 4];
        plaintext[4 * i + 1] = state[i + 1 * 4];
        plaintext[4 * i + 2] = state[i + 2 * 4];
        plaintext[4 * i + 3] = state[i + 3 * 4];
    }

    plaintext
}

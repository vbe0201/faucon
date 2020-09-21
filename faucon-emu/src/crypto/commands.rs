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

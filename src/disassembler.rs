//! Disassembler for the Falcon v5 (fuc5) ISA.

use std::fs::File;
use std::io::Read;
use std::path::Path;

/// Reads the contents of a given binary file into a byte array.
pub fn read_binary<P: AsRef<Path>>(path: P) -> Box<[u8]> {
    let mut binary_buffer = Vec::new();

    let mut file = File::open(path).expect("Failed to read the binary!");
    file.read_to_end(&mut binary_buffer).unwrap();

    binary_buffer.into_boxed_slice()
}

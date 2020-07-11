use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

extern crate faucon_asm;

fn read_binary<P: AsRef<Path>>(file: P) -> Vec<u8> {
    let mut data = Vec::new();

    let mut file = File::open(file).expect("Failed to open binary.");
    file.read_to_end(&mut data).unwrap();

    data
}

fn main() {
    let file = env::args().nth(1).unwrap();
    let binary = &mut &read_binary(file)[..];

    while let Ok(_) = faucon_asm::disassembler::read_instruction(binary) {}
}

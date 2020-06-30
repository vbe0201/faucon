use std::env;
use std::fs::File;
use std::io::{Cursor, Read};
use std::path::Path;

fn read_binary<P: AsRef<Path>>(path: P) -> Cursor<Vec<u8>> {
    let mut file = File::open(path).expect("Failed to open the binary");
    let mut buffer = Vec::new();

    file.read_to_end(&mut buffer).unwrap();

    Cursor::new(buffer)
}

fn main() {
    let binary_path = env::args().nth(1).unwrap();
    let mut binary = read_binary(binary_path);

    for insn in faucon_asm::disassemble(&mut binary).unwrap() {
        println!("{}", insn);
    }
}

use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use cpu::Cpu;

mod cpu;
mod dma;
mod memory;

fn read_binary<P: AsRef<Path>>(path: P) -> Box<[u8]> {
    let mut file = File::open(path).expect("Failed to open the binary");
    let mut buffer = Vec::new();

    file.read_to_end(&mut buffer).unwrap();

    buffer.into_boxed_slice()
}

fn main() {
    let arguments = env::args().collect::<Vec<String>>();
    let binary = read_binary(&arguments[1]);

    let mut cpu = Cpu::new();
}

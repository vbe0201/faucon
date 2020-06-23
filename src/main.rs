use std::convert::TryInto;
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
    while buffer.len() != 0x100 {
        buffer.push(0);
    }

    buffer.into_boxed_slice()
}

fn upload_code(cpu: &mut Cpu, address: u16, vaddress: u32, binary: &[u8]) {
    for (index, mut chunk) in binary.chunks(4).enumerate() {
        cpu.upload_code(
            address + (index << 2) as u16,
            vaddress,
            u32::from_le_bytes(chunk.try_into().unwrap()),
        );
    }
}

fn main() {
    let arguments = env::args().collect::<Vec<String>>();
    let binary = read_binary(&arguments[1]);

    let mut cpu = Cpu::new();
    upload_code(&mut cpu, 0, 0, &binary);

    cpu.step();
}

//! Helpers for reading and uploading executables into the Falcon code segment.

use std::convert::TryInto;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use faucon_emu::cpu::Cpu;

const CODE_ALIGN_BITS: usize = 8;
const CODE_ALIGNMENT: usize = 1 << CODE_ALIGN_BITS;

fn align_up(value: usize, align: usize) -> usize {
    assert!(align.is_power_of_two());

    let mask = align - 1;
    if value & mask == 0 {
        value
    } else {
        (value | mask) + 1
    }
}

fn read_file<P: AsRef<Path>>(path: P) -> Vec<u8> {
    let mut file = File::open(path).expect("Failed to open the binary file");
    let mut contents = Vec::new();

    file.read_to_end(&mut contents).unwrap();

    contents
}

/// Reads a binary at the given path and pads it out to 0x100 byte alignment, if
/// necessary.
///
/// This function guarantees that the received slice of bytes can be mapped
/// into the Falcon IMem correctly. Thus, any code to be uploaded to the Falcon
/// should always be passed through this function.
pub fn read_falcon_binary<P: AsRef<Path>>(path: P) -> Box<[u8]> {
    let mut binary = read_file(path);

    // Falcon code segment can only be modified in 0x100 byte pages.
    // If the binary doesn't have correct alignment, pad it out so
    // the page can be mapped correctly.
    let aligned_len = align_up(binary.len(), CODE_ALIGNMENT);
    while binary.len() < aligned_len {
        binary.push(0);
    }

    binary.into_boxed_slice()
}

/// Uploads a Falcon binary that was obtained from [`read_falcon_binary`] into the
/// code segment of the processor.
///
/// Returns an error if the binary is too large to fit into the Falcon code segment.
///
/// [`read_falcon_binary`]: fn.read_falcon_binary.html
pub fn upload_to_imem(cpu: &mut Cpu, address: u16, vaddress: u32, binary: &[u8]) -> Result<(), ()> {
    assert_eq!((address & 0xFC), 0);
    assert_eq!((vaddress & 0xFC), 0);

    // Check if the binary would fit the Falcon code segment.
    if binary.len() > cpu.imem_size() {
        return Err(());
    }

    for (i, page) in binary.chunks(CODE_ALIGNMENT).enumerate() {
        upload_page_to_imem(
            cpu,
            address + (i << CODE_ALIGN_BITS) as u16,
            vaddress + (i << CODE_ALIGN_BITS) as u32,
            page,
        );
    }

    Ok(())
}

fn upload_page_to_imem(cpu: &mut Cpu, address: u16, vaddress: u32, page: &[u8]) {
    for (offset, word) in page.chunks(4).enumerate() {
        cpu.upload_code(
            address + (offset << 2) as u16,
            vaddress,
            u32::from_le_bytes(word.try_into().unwrap()),
        );
    }
}

use byteorder::{ByteOrder, LittleEndian};

/// The size of a physical memory page.
const PAGE_SIZE: usize = 0x100;

/// A simple memory page of 0x100 bytes in size, used in [`IMem`].
///
/// [`IMem`]: struct.IMem.html
struct Page {
    /// The internal memory buffer for each page.
    memory: [u8; PAGE_SIZE],
}

impl Page {
    /// Creates a new memory page which is zeroed out by default.
    pub fn new() -> Self {
        Page {
            memory: [0; PAGE_SIZE],
        }
    }

    /// Reads a word from a given physical memory address.
    pub fn read(&self, address: u8) -> u32 {
        LittleEndian::read_u32(&self.memory[address as usize..])
    }

    /// Writes a word to a given physical memory address.
    pub fn write(&mut self, address: u8, value: u32) {
        LittleEndian::write_u32(&mut self.memory[address as usize..], value);
    }
}

/// Representation of the raw [`IMem`], consisting of a specific amount of physical
/// memory pages, used for storing code.
///
/// The number of physical pages can be determined through `UC_CAPS & 0x1FF`.
pub struct IMem {
    /// The physical memory pages, used to internally store code.
    pages: Vec<Page>,
}

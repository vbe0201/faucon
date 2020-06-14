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

impl IMem {
    /// Creates a new instance of the physical IMEM.
    ///
    /// The number of available memory pages is calculated through the
    /// `UC_CAPS` MMIO value.
    pub fn new(uc_caps: u32) -> Self {
        // XXX: uc_caps = 128

        // Compute the number of physical pages in the code segment.
        let mut pages_amount = (uc_caps & 0x1FF) as usize;

        // Prepare and initialize the memory pages.
        let mut pages = Vec::with_capacity(pages_amount);
        for _ in 0..pages_amount {
            pages.push(Page::new());
        }

        IMem { pages }
    }

    /// Reads a word from a given page in memory.
    pub fn read(&self, page: usize, offset: u8) -> u32 {
        self.pages[page].read(offset)
    }

    /// Writes a word to a given page in memory.
    pub fn write(&mut self, page: usize, offset: u8, value: u32) {
        self.pages[page].write(offset, value);
    }
}

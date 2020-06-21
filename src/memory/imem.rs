use byteorder::{ByteOrder, LittleEndian};

/// The size of a physical memory page.
const PAGE_SIZE: usize = 0x100;

/// Representation of the raw [`IMem`], consisting of a specific amount of physical
/// memory pages, used for storing code.
///
/// The number of physical pages can be determined through `UC_CAPS & 0x1FF`.
pub struct IMem {
    /// The physical memory pages, used to internally store code.
    pages: Vec<[u8; PAGE_SIZE]>,
}

impl IMem {
    /// Creates a new instance of the physical IMEM.
    ///
    /// The number of available memory pages is calculated through the
    /// value of the `UC_CAPS` register.
    pub fn new() -> Self {
        // XXX: Read the value from the I/O space instead of hardcoding it.
        let uc_caps = 0x20408080;

        // Compute the number of physical pages in the code segment.
        let pages_amount = (uc_caps & 0x1FF) as usize;

        // Prepare and initialize the memory pages.
        let mut pages = Vec::with_capacity(pages_amount);
        for _ in 0..pages_amount {
            pages.push([0; PAGE_SIZE]);
        }

        IMem { pages }
    }

    /// Reads a word from a given page in memory.
    pub fn read(&self, page: u8, offset: u8) -> u32 {
        LittleEndian::read_u32(&self.pages[page as usize][offset as usize..])
    }

    /// Reads a word from a given physical address in memory.
    pub fn read_addr(&self, address: u16) -> u32 {
        self.read((address >> 8) as u8, (address & 0xFF) as u8)
    }

    /// Writes a word to a given page in memory.
    pub fn write(&mut self, page: u8, offset: u8, value: u32) {
        LittleEndian::write_u32(&mut self.pages[page as usize][offset as usize..], value);
    }

    /// Writes a word to a given physical address in memory.
    pub fn write_addr(&mut self, address: u16, value: u32) {
        self.write((address >> 8) as u8, (address & 0xFF) as u8, value);
    }
}

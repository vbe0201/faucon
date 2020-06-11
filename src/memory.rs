//! Implementation of Falcon code and data spaces.

/// The Falcon I/O memory space for data.
///
/// Its size can be determined by looking at the `UC_CAPS` MMIO
/// register. See [`DataSpace::new`] for details.
///
/// This is a linear piece of memory with byte-oriented addressing,
/// used for variables, and stack memory. It can be addressed in
/// 8-bit, 16-bit and 32-bit quantities where unaligned memory
/// access leads to data corruption.
///
/// [`DataSpace::new`]: struct.DataSpace.html#method.new
struct DataSpace {
    /// Internal representation of the data space memory.
    memory: Vec<u8>,
}

impl DataSpace {
    /// Creates a new instance of the data space from the given size.
    ///
    /// The size can be determined through the `UC_CAPS` MMIO:
    /// `UC_CAPS[9:16] << 8`
    pub fn new(size: usize) -> Self {
        DataSpace {
            memory: Vec::with_capacity(size),
        }
    }

    /// Reads a byte from a given address in memory.
    pub fn read_byte(&self, address: u32) -> u8 {
        self.memory[address as usize]
    }

    /// Writes a byte to a given address in memory.
    pub fn write_byte(&mut self, address: u32, value: u8) {
        self.memory[address as usize] = value;
    }

    /// Reads a halfword from a given address in memory.
    pub fn read_halfword(&self, mut address: u32) -> u16 {
        // Enforce aligned memory access.
        address &= !1;

        let low = self.read_byte(address) as u16;
        let high = self.read_byte(address + 1) as u16;

        (high << 8) | low
    }

    /// Writes a halfword to a given address in memory.
    pub fn write_halfword(&mut self, mut address: u32, mut value: u16) {
        // If the address is unaligned, fuck up the written value.
        if address & 1 != 0 {
            value = (value & 0xFF) << (address as u16 & 1) * 8;
        }

        // Enforce aligned memory access.
        address &= !1;

        self.write_byte(address, value as u8);
        self.write_byte(address + 1, (value >> 8) as u8);
    }

    /// Reads a word from a given address in memory.
    pub fn read_word(&self, mut address: u32) -> u32 {
        // Enforce aligned memory access.
        address &= !3;

        let low = self.read_halfword(address) as u32;
        let high = self.read_halfword(address + 2) as u32;

        (high << 16) | low
    }

    /// Writes a word to a given address in memory.
    pub fn write_word(&mut self, mut address: u32, mut value: u32) {
        // If the address is unaligned, fuck up the written value.
        if address & 1 != 0 {
            value = (value & 0xFF) << (address & 3) * 8;
        } else if address & 2 != 0 {
            value = (value & 0xFFFF) << (address & 3) * 8;
        }

        // Enforce aligned memory access.
        address &= !3;

        self.write_halfword(address, value as u16);
        self.write_halfword(address + 2, (value >> 16) as u16);
    }
}

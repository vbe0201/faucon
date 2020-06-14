use byteorder::{ByteOrder, LittleEndian};

/// The Falcon memory space for data.
///
/// Its size can be determined by looking at the `UC_CAPS` MMIO
/// register. See [`DMem::new`] for details.
///
/// This is a linear piece of memory with byte-oriented addressing,
/// used for variables, and stack memory. It can be addressed in
/// 8-bit, 16-bit and 32-bit quantities where unaligned memory
/// access leads to data corruption.
///
/// [`DMem::new`]: struct.DMem.html#method.new
#[derive(Debug)]
pub struct DMem {
    /// Internal representation of the data space memory.
    memory: Vec<u8>,
}

impl DMem {
    /// Creates a new instance of the data space from the given size.
    ///
    /// The size can be determined through `(UC_CAPS >> 9 & 0x1FF) << 8`.
    pub fn new(size: usize) -> Self {
        // Initialize and zero out the memory.
        // XXX: Use (UC_CAPS >> 9 & 0x1FF) << 8 to calculate DMem size instead of hardcoding it.
        let mut memory = Vec::with_capacity(0x4000);
        for _ in 0..size {
            memory.push(0);
        }

        DMem { memory }
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

        LittleEndian::read_u16(&self.memory[address as usize..])
    }

    /// Writes a halfword to a given address in memory.
    pub fn write_halfword(&mut self, mut address: u32, mut value: u16) {
        // If the address is unaligned, fuck up the written value.
        if (address & 1) != 0 {
            value = (value & 0xFF) << (address as u16 & 1) * 8;
        }

        // Enforce aligned memory access.
        address &= !1;

        LittleEndian::write_u16(&mut self.memory[address as usize..], value);
    }

    /// Reads a word from a given address in memory.
    pub fn read_word(&self, mut address: u32) -> u32 {
        // Enforce aligned memory access.
        address &= !3;

        LittleEndian::read_u32(&self.memory[address as usize..])
    }

    /// Writes a word to a given address in memory.
    pub fn write_word(&mut self, mut address: u32, mut value: u32) {
        // If the address is unaligned, fuck up the written value.
        if (address & 1) != 0 {
            value = (value & 0xFF) << (address & 3) * 8;
        } else if (address & 2) != 0 {
            value = (value & 0xFFFF) << (address & 3) * 8;
        }

        // Enforce aligned memory access.
        address &= !3;

        LittleEndian::write_u32(&mut self.memory[address as usize..], value);
    }
}

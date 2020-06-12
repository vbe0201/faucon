//! Implementation of Falcon code and data spaces.

/// The fixed size of a code memory page in bytes.
const PAGE_SIZE: usize = 0x100;

/// Representation of Falcon memory space.
///
/// It mainly consists of a [`DataSpace`] for the stack and program
/// variables, but also a separate code space for program code,
/// managed through a hidden [`Tlb`].
///
/// [`DataSpace`]: struct.DataSpace.html
/// [`Tlb`]: struct.Tlb.html
pub struct Memory {
    /// The Falcon data space in memory.
    ///
    /// See [`DataSpace`] for details.
    ///
    /// [`DataSpace`]: struct.DataSpace.html
    pub data: DataSpace,
    /// The Falcon TLB table responsible for mapping physical to virtual memory.
    ///
    /// This is used for the code space.
    pub tlb: Tlb,
}

/// The Falcon Translation Lookaside Buffer for mapping code pages in memory.
///
/// It consists of multiple [`TlbCell`]s, each entry representing one physical
/// page. The number of physical pages can be determined through `UC_CAPS[0:8]`.
///
/// The valid virtual address range is set as `0..(1 << UC_CAPS2[16:19]) * 0x100`
/// and whenever such an address is accessed, the TLB searches for a corresponding
/// [`TlbCell`] entry. If there is more than one match or no matches at all, it
/// is considered an error and a trap should be generated through [`Cpu::trap`].
///
/// [`TlbCell`]: struct.TlbCell.html
/// [`Cpu::trap`]: ../cpu/struct.Cpu.html#method.trap
pub struct Tlb {
    /// The TLB entries that represent physical pages.
    pub entries: Vec<TlbCell>,
}

/// [`TlbCell`] flag bits for physical memory pages.
///
/// [`TlbCell`]: struct.TlbCell.html
#[derive(Debug)]
#[repr(u8)]
pub enum PageFlag {
    /// Indicates that the page is mapped and complete and can be used.
    USABLE = 1 << 0,
    /// Indicates that the page is mapped but code is still being uploaded.
    BUSY = 1 << 1,
    /// Indicates that the page contains secret code.
    SECRET = 1 << 2,
}

/// An entry in the [`Tlb`] that represents a physical code page.
///
/// [`Tlb`]: struct.Tlb.html
pub struct TlbCell {
    pub memory: [u8; PAGE_SIZE],
    virtual_page_index: usize,
    flags: u8,
}

impl TlbCell {
    /// Toggles a flag in the page settings based on the value of `set`.
    ///
    /// - `set = true` sets the given flag
    /// - `set = false` clears the given flag
    pub fn set_flag(&mut self, flag: PageFlag, set: bool) {
        if set {
            self.flags |= flag as u8;
        } else {
            self.flags &= !(flag as u8);
        }
    }

    /// Gets a flag from the page settings and indicates whether it is set.
    pub fn get_flag(&self, flag: PageFlag) -> bool {
        (self.flags & flag as u8) != 0
    }

    /// Checks if the [`Tlb`] entry is considered valid.
    ///
    /// [`Tlb`]: struct.Tlb.html
    pub fn is_valid(&self) -> bool {
        // Pages are valid when at least one of the highest 3 bits is set.
        (self.flags & 0x7) != 0
    }
}

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
#[derive(Debug)]
pub struct DataSpace {
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

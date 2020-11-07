//! Implementation of Falcon code and data memory in SRAM.

mod tlb;

use byteorder::{ByteOrder, LittleEndian};

pub use tlb::*;

/// The size of a physical memory page in Falcon code space.
pub const PAGE_SIZE: usize = 0x100;

/// Representation of the Falcon memory space.
///
/// It consists of separate memory spaces for data and code,
/// where data is stored in a flat piece of memory and code
/// in virtual memory pages, translated by a hidden TLB.
pub struct Memory {
    /// Representation of the Falcon data space.
    ///
    /// This is a linear piece of memory with byte-oriented addressing,
    /// used for variables and stack memory. It can be addressed in 8-bit,
    /// 16-bit and 32-bit quantities where unaligned memory access leads
    /// to data corruption.
    pub data: Vec<u8>,
    /// Representation of the Falcon code space.
    ///
    /// Code segment uses primitive paging in 0x100 byte pages.
    /// Address translation is done in hidden TLB memory, with one entry
    /// for each physical page.
    pub code: Vec<u8>,
    /// Representation of the hidden Falcon TLB.
    ///
    /// The TLB is used for address translation via an array of entries,
    /// each representing a physical page index.
    pub tlb: Tlb,
}

impl Memory {
    /// Creates a new instance of the memory, initialized to all zeroes by
    /// default.
    pub fn new(imem_size: u32, dmem_size: u32) -> Self {
        Memory {
            data: vec![0; dmem_size as usize],
            code: vec![0; imem_size as usize],
            tlb: Tlb::new(),
        }
    }

    /// Reads a byte from a given address in Falcon data space.
    pub fn read_data_byte(&self, address: u32) -> u8 {
        self.data[address as usize]
    }

    /// Reads a halfword from a given address in Falcon data space.
    pub fn read_data_halfword(&self, mut address: u32) -> u16 {
        // Enforce aligned memory access.
        address &= !1;

        LittleEndian::read_u16(&self.data[address as usize..])
    }

    /// Reads a word from a given address in Falcon data space.
    pub fn read_data_word(&self, mut address: u32) -> u32 {
        // Enforce aligned memory access.
        address &= !3;

        LittleEndian::read_u32(&self.data[address as usize..])
    }

    /// Reads a word from a given physical address in code space.
    pub fn read_code_addr(&self, address: u16) -> u32 {
        LittleEndian::read_u32(&self.code[address as usize..])
    }

    /// Writes a byte to a given address in Falcon data space.
    pub fn write_data_byte(&mut self, address: u32, value: u8) {
        self.data[address as usize] = value;
    }

    /// Writes a halfword to a given address in Falcon data space.
    pub fn write_data_halfword(&mut self, mut address: u32, mut value: u16) {
        // If the address is unaligned, fuck up the written value.
        if (address & 1) != 0 {
            value = (value & 0xFF) << (address as u16 & 1) * 8;
        }

        // Enforce aligned memory access.
        address &= !1;

        LittleEndian::write_u16(&mut self.data[address as usize..], value);
    }

    /// Writes a word to a given address in Falcon data space.
    pub fn write_data_word(&mut self, mut address: u32, mut value: u32) {
        // If the address is unaligned, fuck up the written value.
        if (address & 1) != 0 {
            value = (value & 0xFF) << (address & 3) * 8;
        } else if (address & 2) != 0 {
            value = (value & 0xFFFF) << (address & 3) * 8;
        }

        // Enforce aligned memory access.
        address &= !3;

        LittleEndian::write_u32(&mut self.data[address as usize..], value);
    }

    /// Reads a code word from a given physical address in code space.
    pub fn read_code(&self, address: u16) -> u32 {
        LittleEndian::read_u32(&self.code[address as usize..])
    }

    /// Writes a code word to a given physical address in code space.
    pub fn write_code(&mut self, address: u16, value: u32) {
        LittleEndian::write_u32(&mut self.code[address as usize..], value);
    }
}

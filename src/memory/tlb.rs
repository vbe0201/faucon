/// [`TlbCell`] flag bits for physical memory pages.
///
/// [`TlbCell`]: struct.TlbCell.html
#[derive(Debug)]
#[repr(u8)]
pub enum PageFlag {
    /// Indicates that the page is mapped and complete and can be used.
    Usable = 1 << 0,
    /// Indicates that the page is mapped but code is still being uploaded.
    Busy = 1 << 1,
    /// Indicates that the page contains secret code.
    Secret = 1 << 2,
}

/// The Falcon Translation Lookaside Buffer for mapping code pages in memory.
///
/// It consists of multiple [`TlbCell`]s, each entry representing one physical
/// page. The number of physical pages can be determined through `UC_CAPS & 0xFF`.
///
/// The valid virtual address range is set as `0..(1 << (UC_CAPS2 >> 16 & 0xF)) * 0x100`
/// and whenever such an address is accessed, the TLB searches for a corresponding
/// [`TlbCell`] entry. If there is more than one match or no matches at all, it
/// is considered an error and a trap should be generated through [`Cpu::trap`].
///
/// [`TlbCell`]: struct.TlbCell.html
/// [`Cpu::trap`]: ../../cpu/struct.Cpu.html#method.trap
struct Tlb {
    /// The entries of the TLB, each representing a physical page.
    entries: Vec<TlbCell>,
}

impl Tlb {
    /// Creates a new instance of the Translation Lookaside Buffer for the
    /// amount of physical memory pages implied by the value of `UC_CAPS`
    /// register.
    pub fn new() -> Self {
        // XXX: Read the value from the I/O space instead of hardcoding it.
        let uc_caps = 0x20408080;

        // Compute the number of physical pages in the code segment.
        let mut pages_amount = (uc_caps & 0x1FF) as usize;

        // Initialize the TLB entries to empty.
        let mut entries = Vec::with_capacity(pages_amount);
        for _ in 0..pages_amount {
            entries.push(TlbCell::new());
        }

        Tlb { entries }
    }
}

/// An entry in the [`Tlb`] that represents a physical code page.
///
/// [`Tlb`]: struct.Tlb.html
pub struct TlbCell {
    /// The virtual page number corresponding to the physical page.
    pub virtual_page_number: u16,
    /// The status flag bits for a physical page.
    flags: u8,
}

impl TlbCell {
    /// Creates a new entry for the TLB, marked as completely free and unmapped.
    pub fn new() -> Self {
        TlbCell {
            virtual_page_number: 0,
            flags: 0,
        }
    }

    /// Maps the physical page corresponding to the TLB entry to the virtual page
    /// space the given address belongs to.
    ///
    /// NOTE: This sets [`PageFlag::Busy`] for this entry. It is within the caller's
    /// responsibility to upload code and change the flag status before using the page.
    ///
    /// [`PageFlag::Busy`]: enum.PageFlag.html#variant.Busy
    pub fn map(&mut self, address: u16) {
        // XXX: Calculate the value through `UC_CAPS2 >> 16 & 0xF`.
        let vm_pages_log2 = 8;

        // Calculate and apply the new TLB settings.
        self.virtual_page_number = (address >> 8) & ((1 << vm_pages_log2) - 1);
        self.set_flag(PageFlag::Busy, true);
    }

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
        self.flags != 0
    }

    /// Indicates whether the physical page corresponding to the TLB entry
    /// is currently unmapped.
    pub fn is_free(&self) -> bool {
        self.virtual_page_number == 0 && self.flags == 0
    }

    /// Clears the TLB entry and frees it for remapping.
    ///
    /// NOTE: Pages containing secret code can not be cleared.
    /// The page has to be re-uploaded with non-secret data first.
    pub fn clear(&mut self) {
        if !self.get_flag(PageFlag::Secret) {
            self.virtual_page_number = 0;
            self.flags = 0;
        }
    }

    /// Gets the numeral representation of the TLB entry.
    ///
    /// The format of the result is:
    /// - Bits 31:27: 0
    /// - Bits 26:24: flags
    /// - Bits 23:8:  virtual page number
    /// - Bits 7:0:   0
    pub fn get(&self) -> u32 {
        (self.flags << 24) as u32 | (self.virtual_page_number << 8) as u32
    }
}

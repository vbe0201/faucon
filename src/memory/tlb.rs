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

/// Potential TLB lookup errors.
///
/// These may occur when doing virtual <-> physical page translations.
#[derive(Debug)]
pub enum LookupError {
    /// A page fault that occurs when multiple TLB entries could be matched
    /// for a single physical page.
    MultiplePageHits,
    /// A page fault that occurs when no TLB entries could be matched for a
    /// physical page.
    NoPageHits,
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
pub struct Tlb {
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
        let pages_amount = (uc_caps & 0x1FF) as usize;

        // Initialize the TLB entries to empty.
        let mut entries = Vec::with_capacity(pages_amount);
        for _ in 0..pages_amount {
            entries.push(TlbCell::new());
        }

        Tlb { entries }
    }

    /// Gets the [`TlbCell`] that corresponds to the given physical page index.
    ///
    /// NOTE: It is within the caller's responsibilities to ensure that the
    /// page index is actually valid, otherwise bad things will happen.
    ///
    /// [`TlbCell`]: struct.TlbCell.html
    pub fn get(&mut self, page: u8) -> &mut TlbCell {
        &mut self.entries[page as usize]
    }

    /// Finds a [`TlbCell`] that corresponds to the given virtual address.
    ///
    /// If a page fault occurs, a [`LookupError`] is returned.
    ///
    /// In case the page is found, a `(index, entry)` is returned, where
    /// `index` is the physical page index corresponding to the [`TlbCell`]
    /// denoted by `entry`.
    ///
    /// [`TlbCell`]: struct.TlbCell.html
    /// [`LookupError`]: enum.LookupError.html
    pub fn find(&mut self, address: u32) -> Result<(usize, &mut TlbCell), LookupError> {
        // XXX: Calculate the value through `UC_CAPS2 >> 16 & 0xF`.
        let vm_pages_log2 = 8;

        // Calculate the virtual page number to look up.
        let page_index = (address >> 8) as u16 & ((1 << vm_pages_log2) - 1);

        // Find all the valid entries that match the virtual page number.
        let mut entries = self
            .entries
            .iter_mut()
            .enumerate()
            .filter(|(_, e)| e.is_valid() && e.virtual_page_number == page_index);

        // Count the hits and determine the appropriate result based on that.
        let hits = entries.by_ref().count();
        if hits == 1 {
            Ok(entries.next().unwrap())
        } else if hits == 0 {
            Err(LookupError::NoPageHits)
        } else {
            Err(LookupError::MultiplePageHits)
        }
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
    pub fn map(&mut self, address: u32) {
        // XXX: Calculate the value through `UC_CAPS2 >> 16 & 0xF`.
        let vm_pages_log2 = 8;

        // Calculate and apply the new TLB settings.
        self.virtual_page_number = (address >> 8) as u16 & ((1 << vm_pages_log2) - 1);
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

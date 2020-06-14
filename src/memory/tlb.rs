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

/// An entry in the [`Tlb`] that represents a physical code page.
///
/// [`Tlb`]: struct.Tlb.html
pub struct TlbCell {
    /// The virtual page number corresponding to the physical page.
    pub virtual_page_number: u16,
    /// The status flag bits for a physical page.
    flags: u8,
}

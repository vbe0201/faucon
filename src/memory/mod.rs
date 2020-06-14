//! Implementation of the Falcon memory space.

mod dmem;
mod imem;
mod tlb;

/// Representation of the Falcon memory space.
///
/// It consists of separate memory spaces for data and code,
/// where data is stored in a flat piece of memory and code
/// in virtual memory pages, translated by a hidden TLB.
pub struct Memory {
    /// The Falcon code space.
    pub code: imem::IMem,
    /// The Falcon data space.
    pub data: dmem::DMem,
    /// The hidden TLB, responsible for virtual <-> physical address
    /// translation.
    tlb: tlb::Tlb,
}

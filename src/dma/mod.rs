//! Implementation of the Falcon DMA engine.

use crate::dma::RequestMode::CodeLoad;
use crate::memory::*;

/// Supported request modes that the DMA engine can process.
#[derive(Debug, PartialEq)]
pub enum RequestMode {
    /// A DMA request to load Falcon code from external memory.
    CodeLoad,
    /// A DMA request to load Falcon data from external memory.
    DataLoad,
    /// A DMA request to store Falcon data in external memory.
    DataStore,
}

/// A Falcon DMA request to perform a code/data transfer.
// TODO: Figure out the missing secret flag.
#[derive(Debug)]
pub struct Request {
    pub mode: RequestMode,
    external_port: u8,
    external_base: u32,
    external_offset: u32,
    local_address: u16,
    size: Option<u8>,
    secret: Option<bool>,
}

impl Request {
    /// Gets the port and the start address of the external party for the xfer
    /// operation.
    pub fn external_party(&self) -> (u8, usize) {
        // The external offset always has to be aligned to the xfer size.
        assert_eq!(self.external_offset % self.xfer_size() as u32, 0);

        (
            self.external_port,
            ((self.external_base << 8) + self.external_offset) as usize,
        )
    }

    /// Gets the virtual destination address for code xfers.
    pub fn vaddr(&self) -> u32 {
        // The external offset always has to be aligned to the xfer size.
        assert_eq!(self.external_offset % self.xfer_size() as u32, 0);

        // Since the external offset also represents the virtual address
        // to be used in Falcon IMEM, return it as such.
        self.external_offset
    }

    /// The physical start address of the local party for the xfer operation.
    pub fn local_party(&self) -> u16 {
        // The local address always has to be aligned to the xfer size.
        assert_eq!(self.local_address % self.xfer_size() as u16, 0);

        self.local_address
    }

    /// Gets the xfer size that indicates how much data to transfer.
    ///
    /// The actual amount of bytes to copy can be obtained through
    /// [`Request::xfer_data_size`].
    ///
    /// [`Request::xfer_data_size`]: struct.Request.html#method.xfer_data_size
    pub fn xfer_size(&self) -> u8 {
        if self.mode == RequestMode::CodeLoad {
            // For code xfers, the size is effectively always 6.
            6
        } else {
            // For data xfers, the size must be within a 0..=6
            // range and cannot be empty.
            let value = self.size.unwrap();
            assert!(value <= 6);

            value
        }
    }

    /// Gets the amount of bytes to copy in the xfer.
    pub fn xfer_data_size(&self) -> usize {
        (4 << self.xfer_size()) as usize
    }

    /// Checks whether the xfer is enhanced by cryptographic functionality.
    pub fn secret(&self) -> bool {
        if self.mode == CodeLoad {
            // In case of a code load, the secret flag may or may not be set.
            self.secret.unwrap()
        } else {
            // For data transfers, secret xfers are irrelevant, thus always
            // being set to `false`.
            false
        }
    }
}

// TODO: Implement DMA engine functionality.

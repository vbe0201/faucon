//! Implementation of the Falcon DMA engine.

use std::convert::TryInto;
use std::ptr;

use crate::cpu::Cpu;

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
    /// Constructs a new DMA request.
    pub fn new(
        mode: RequestMode,
        external_port: u8,
        external_base: u32,
        external_offset: u32,
        local_address: u16,
        size: Option<u8>,
        secret: Option<bool>,
    ) -> Self {
        Request {
            mode,
            external_port,
            external_base,
            external_offset,
            local_address,
            size,
            secret,
        }
    }
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
        if self.mode == RequestMode::CodeLoad {
            // In case of a code load, the secret flag may or may not be set.
            self.secret.unwrap()
        } else {
            // For data transfers, secret xfers are irrelevant, thus always
            // being set to `false`.
            false
        }
    }
}

/// Representation of the Falcon DMA engine.
///
/// The internal controller allows for asynchronous copies between Falcon DMEM/IMEM
/// and external memory, issued through DMA [`Request`]s.
// TODO: Make DMA engine capable of processing request asynchronously in separate threads.
#[derive(Debug)]
pub struct Engine {
    /// A queue of DMA [`Request`]s to be processed by the engine.
    queue: Vec<Request>,
}

impl Engine {
    /// Creates a new instance of the DMA engine.
    pub fn new() -> Self {
        Engine { queue: Vec::new() }
    }

    /// Checks whether the DMA engine is currently busy processing
    /// requests.
    pub fn is_busy(&self) -> bool {
        // TODO
        false
    }

    /// Enqueues a new [`Request`] in the DMA queue.
    ///
    /// # Safety
    ///
    /// Due to raw pointer arithmetic used when processing a request,
    /// the user must ensure that all the memory addresses and offsets
    /// denoted in a request are valid and aligned, otherwise undefined
    /// behavior will be triggered.
    pub unsafe fn enqueue(&mut self, request: Request, cpu: &mut Cpu) {
        self.queue.push(request);

        // TODO
        self.process_request(cpu);
    }

    /// Executes a DMA request.
    unsafe fn process_request(&mut self, cpu: &mut Cpu) {
        if let Some(request) = self.queue.pop() {
            match request.mode {
                RequestMode::CodeLoad => {
                    let destination = request.local_party();
                    let (_, source) = request.external_party();
                    let size = request.xfer_data_size();

                    // TODO: Add support for secret xfers.

                    // Copy the code to a vector for more idiomatic interaction with it.
                    let mut data = Vec::with_capacity(size);
                    ptr::copy_nonoverlapping(source as *const u8, data.as_mut_ptr(), size);

                    for (index, chunk) in data.chunks(4).enumerate() {
                        cpu.upload_code(
                            destination + (index << 2) as u16,
                            request.vaddr(),
                            u32::from_le_bytes(chunk.try_into().unwrap()),
                        )
                    }
                }
                RequestMode::DataLoad => todo!("Implement this"),
                RequestMode::DataStore => todo!("Implement this"),
            }
        }
    }
}

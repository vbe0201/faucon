//! Implementation of the Falcon DMA engine.

use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;

use crate::cpu::Cpu;
use crate::dma::RequestMode::CodeLoad;
use crate::memory::{Memory, PageFlag};

/// Supported modes of DMA requests that can be executed.
#[derive(Debug, PartialEq)]
pub enum RequestMode {
    /// A DMA request to load code from memory.
    CodeLoad,
    /// A DMA request to load data from memory.
    DataLoad,
    /// A DMA request to store data in memory.
    DataStore,
}

/// A Falcon DMA engine.
///
/// These are being processed asynchronously by the DMA [`Engine`].
///
/// [`Engine`]: struct.Engine.html
#[derive(Debug, PartialEq)]
struct Request {
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
    pub fn virtual_address(&self) -> u32 {
        // The external offset always has to be aligned to the xfer size.
        assert_eq!(self.external_offset % self.xfer_size() as u32, 0);

        // Since the external offset also represents the virtual address
        // in Falcon IMEM, return it as such.
        self.external_offset
    }

    /// The physical start address of the local party for the xfer operation.
    pub fn local_party(&self) -> u16 {
        assert_eq!(self.local_address % self.xfer_size() as u16, 0);

        self.local_address
    }

    /// Gets the xfer size that indicates how much data to copy.
    ///
    /// The actual data size can be obtained through [`Request::xfer_data_size`].
    ///
    /// [`Request::xfer_data_size`]: struct.Request.html#method.xfer_data_size
    pub fn xfer_size(&self) -> u8 {
        if self.mode == RequestMode::CodeLoad {
            // For code xfers, the size is effectively always 6.
            6
        } else {
            // For data xfers, the size must be within a 0..=6
            // range and cannot be None.
            let value = self.size.unwrap();
            assert!(value <= 6);

            value
        }
    }

    /// Gets the amount of bytes to process in this xfer request.
    pub fn xfer_data_size(&self) -> u16 {
        (4 << self.xfer_size()) as u16
    }

    /// Checks whether the xfer should use cryptographic secrets.
    pub fn secret_xfer(&self) -> bool {
        if self.mode == CodeLoad {
            // In case of a code load, the secret flag may or may not be set.
            self.secret.unwrap()
        } else {
            // For data transfers, secret xfers are irrelevant, thus always
            // being `false`.
            false
        }
    }
}

/// Representation of the Falcon DMA engine.
///
/// The internal controller allows for asynchronous copies between Falcon DMEM/IMEM
/// and external memory, issued through [`Request`]s.
///
/// [`Request`]: struct.Request.html
#[derive(Debug)]
pub struct Engine {
    /// Atomic boolean used for indicating a DMA engine shutdown.
    run: AtomicBool,
    /// The queue to store DMA [`Request`]s.
    ///
    /// [`Request`]: struct.Request.html
    queue: Mutex<Vec<Request>>,
}

// TODO: Implement is_busy().
impl Engine {
    /// Constructs a new instance of the DMA engine.
    pub fn new() -> Self {
        Engine {
            run: AtomicBool::new(true),
            queue: Mutex::new(Vec::new()),
        }
    }

    /// Shuts down the DMA engine.
    ///
    /// This forcefully cancels all pending DMA [`Request`]s.
    ///
    /// [`Request`]: struct.Request.html
    pub fn shutdown(&self) {
        self.run.store(false, Ordering::SeqCst);
    }

    /// Adds a new DMA [`Request`] to the internal queue.
    ///
    /// [`Request`]: struct.Request.html
    pub fn enqueue(&self, request: Request) {
        let mut queue = self.queue.lock().unwrap();
        queue.push(request);
    }
}

pub unsafe fn execute(dma: Arc<Engine>, cpu: Arc<Cpu>) -> thread::JoinHandle<()> {
    thread::spawn(move || loop {
        // When the DMA engine is shutting down, opt out of the loop.
        if !dma.run.load(Ordering::Relaxed) {
            break;
        }

        let mut queue = dma.queue.lock().unwrap();
        if let Some(request) = queue.pop() {
            let mut memory = cpu.memory.lock().unwrap();

            // TODO: Process the DMA request.
            match request.mode {
                RequestMode::CodeLoad => {
                    let (_, source) = request.external_party();
                    let destination = request.local_party();
                    let size = request.xfer_data_size() as usize;

                    // TODO: Add support for secret xfers.

                    // Get the corresponding TLB entry to the given virtual address.
                    let mut tlb = memory.tlb.get((destination >> 8) as u8);

                    // Copy the code to the `data` vector for more idiomatic
                    // completion of the operation with emulated memory.
                    let mut data = Vec::with_capacity(size);
                    ptr::copy_nonoverlapping(source as *const u8, data.as_mut_ptr(), size);

                    // Map a new page for the given virtual address.
                    tlb.map(request.virtual_address());

                    // Upload the code.
                    for (index, chunk) in data.chunks(4).enumerate() {
                        memory.code.write_addr(
                            destination + (index << 2) as u16,
                            u32::from_le_bytes(chunk),
                        );
                    }

                    // Mark the page usable, now that the code transfer is complete.
                    tlb.set_flag(PageFlag::Usable, true);
                }
                RequestMode::DataLoad => {}
                RequestMode::DataStore => {}
            }
        }
    })
}

use std::io::{self, Read, Write};

use crate::{FalconError, Instruction};

/// A disassembler that pretty-prints assembly instruction mnemonics with their
/// raw bytes and memory addresses.
pub struct Disassembler<W> {
    // The base address in memory at which the disassembler begins.
    base: u32,
    // The sink to output disassembled and formatted instructions to.
    output: TrackWrite<W>,
}

impl<W> Disassembler<W> {
    /// Creates a new `Disassembler` that will write the disassembled code
    /// to the given output.
    pub fn new(output: W) -> Self {
        Self {
            base: 0,
            output: TrackWrite::new(output),
        }
    }

    /// Sets the starting address to disassemble from to the given base.
    pub fn with_base(mut self, base: u32) -> Self {
        self.base = base;
        self
    }
}

impl Disassembler<io::Stdout> {
    /// Creates a new disassembler that will print out the disassembled
    /// code to stdout.
    pub fn stdout() -> Self {
        Self::new(io::stdout())
    }
}

impl<W: Write> Disassembler<W> {
    /// Disassembles a stream of bytes.
    ///
    /// This method will not return an error if an unknown or invalid instruction
    /// is encountered, but instead just prints `[aborted] (invalid instruction)`
    /// to the output stream.
    ///
    /// It will also ignore any errors that occurred while reading from `stream`.
    pub fn disassemble_stream<R: Read>(&mut self, stream: &mut R) -> io::Result<()> {
        let mut pc = 0;
        let insns = std::iter::from_fn(|| match super::read_instruction(stream, &mut pc) {
            Ok(res) => Some(res),
            Err(FalconError::InvalidOpcode(op)) => {
                println!("[aborted] (unknown instruction '{:x}')", op);
                None
            }
            Err(FalconError::IoError(_)) | Err(FalconError::ParseError(_)) => None,
            Err(FalconError::Eof) => {
                println!("[aborted] (end of file)");
                None
            }
        });
        self.disassemble(insns)
    }

    /// Disassembles a list of instructions and writes them to the output of this
    /// Disassembler.
    ///
    /// This method will not reset the inner pc, so if you call it multiple times,
    /// the addresses will still increase.
    ///
    /// # Panics
    ///
    /// If the [`Instruction`] objects yielded by the iterator do not have their
    /// raw byte representations assigned to them via [`Instruction::with_raw_bytes`],
    /// this method will panic.
    pub fn disassemble<'a>(
        &mut self,
        insns: impl IntoIterator<Item = Instruction>,
    ) -> io::Result<()> {
        let out = &mut self.output;
        for insn in insns {
            out.reset();
            write!(out, "{:08x} ", self.base + insn.program_counter())?;

            insn.raw_bytes()
                .unwrap()
                .iter()
                .try_for_each(|byte| write!(out, "{:02x} ", byte))?;
            align_to(out, out.count, 32)?;
            writeln!(out, "{}", insn)?;
        }

        Ok(())
    }
}

fn align_to<W: Write>(out: &mut W, current: usize, width: usize) -> io::Result<()> {
    (current..width).try_for_each(|_| write!(out, " "))
}

struct TrackWrite<W> {
    count: usize,
    inner: W,
}

impl<W> TrackWrite<W> {
    fn new(inner: W) -> Self {
        Self { inner, count: 0 }
    }

    fn reset(&mut self) {
        self.count = 0;
    }
}

impl<W: Write> io::Write for TrackWrite<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let count = self.inner.write(buf)?;
        self.count += count;
        Ok(count)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

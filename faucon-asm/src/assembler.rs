//! Assembler for the Falcon ISA.

mod context;
pub mod error;
mod lexer;
mod parser;
pub mod span;

use std::path::Path;

use crate::FalconError;
pub use error::*;
pub(crate) use lexer::Token;
pub use span::*;

/// Assembler for building Falcon machine code out of human-readable assembly
/// language.
pub struct Assembler<'a> {
    include_path: Vec<&'a Path>,
}

impl<'a> Assembler<'a> {
    /// Constructs a new assembler over a given main source file.
    pub fn new() -> Self {
        Assembler {
            include_path: Vec::new(),
        }
    }

    /// Consumes the assembler and extends the include path with the additionally
    /// supplied path elements.
    ///
    /// The include path is an internal cache of directory paths which will be
    /// used to track down relative file paths to include in the main assembly
    /// source code.
    ///
    /// This implementation actively sorts out all duplicate path elements, from
    /// the ones passed to this method but also the ones already loaded in, and
    /// extends the internal cache by the new elements. This avoids conflicts
    /// when including a source file that resolves to multiple hits as a result
    /// of having the same paths scanned multiple times.
    pub fn with_include_path(mut self, paths: Vec<&'a Path>) -> Self {
        let mut already_seen = Vec::with_capacity(self.include_path.len() + paths.len());
        already_seen.extend(self.include_path.iter());
        self.include_path
            .retain(|item| match already_seen.contains(item) {
                true => false,
                _ => {
                    already_seen.push(item.clone());
                    true
                }
            });
        self
    }

    /// Consumes the assembler into building Falcon machine code using the Assembly
    /// input supplied as a file path to assemble.
    ///
    /// This file may include and utilize all symbols from source files in the
    /// internal include path.
    pub fn assemble<P: AsRef<Path>>(self, file: P) -> Result<Vec<u8>, FalconError> {
        todo!()
    }
}

//! Assembler for the Falcon ISA.

pub(crate) mod codegen;
mod context;
pub mod error;
mod lexer;
mod parser;
pub mod span;

use std::ffi::OsString;
use std::fs;
use std::iter::Peekable;
use std::path::Path;

use crate::FalconError;
use context::{Context, Directive};
pub use error::*;
pub(crate) use lexer::Token;
pub use span::*;

/// Assembler for building Falcon machine code out of human-readable assembly
/// language.
pub struct Assembler<'a> {
    include_path: Vec<&'a Path>,
    asm_context: Context<'a>,
}

impl<'a> Assembler<'a> {
    /// Constructs a new assembler over a given main source file.
    pub fn new() -> Self {
        Assembler {
            include_path: Vec::new(),
            asm_context: Context::new(),
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

    fn populate_main_context<I>(&mut self, mut iter: Peekable<I>) -> Result<(), ParseError>
    where
        I: Iterator<Item = ParseSpan<Token<'a>>>,
    {
        loop {
            if let Some(span) = iter.next() {
                match span.token() {
                    Token::Directive(d) => {
                        match context::parse_directive(d, &mut iter).map_err(|e| {
                            ParseError::build_unexpected_token_error(e.unwrap_or(span.clone()))
                        })? {
                            Directive::Equ(name, value) => {
                                self.asm_context
                                    .add_declaration(name, value)
                                    .map_err(|_| ParseError::build_redefined_symbol_error(span))?;
                            }
                            Directive::Include(_) => todo!(),
                            Directive::Section(mode, name, addr) => {
                                self.asm_context.add_section(name, mode, addr);
                            }
                            dir => {
                                self.asm_context.add_directive(dir);
                                self.asm_context.current_section().add_code_token(span);
                            }
                        }
                    }
                    t => {
                        if let Token::Label(l) = t {
                            self.asm_context.add_label(l).map_err(|_| {
                                ParseError::build_redefined_symbol_error(span.clone())
                            })?;
                        }
                        self.asm_context.current_section().add_code_token(span);
                    }
                }
            } else {
                break;
            }
        }

        Ok(())
    }

    /// Consumes the assembler into building Falcon machine code using the Assembly
    /// input supplied as a file path to assemble.
    ///
    /// This file may include and utilize all symbols from source files in the
    /// internal include path.
    pub fn assemble<P: 'a + AsRef<Path>>(self, file: P) -> Result<Vec<u8>, FalconError> {
        let file = file.as_ref();
        let source = fs::read_to_string(file).map_err(FalconError::IoError)?;

        let context_name = file
            .file_name()
            .and_then(|s| Some(s.to_os_string()))
            .unwrap();
        self.assemble_str(&source, &context_name)
    }

    /// Consumes the assembler into building Falcon machine code out of the given
    /// Assembly source code string.
    ///
    /// The `context_name` argument should ideally be the name of the source file to
    /// assemble. If not present, pass `"<<main>>"` instead.
    ///
    /// The code may include and utilize all symbols from source files in the
    /// internal include path.
    pub fn assemble_str(
        mut self,
        source: &'a str,
        context_name: &'a OsString,
    ) -> Result<Vec<u8>, FalconError> {
        let tokens = lexer::tokenize(source, context_name)
            .map_err(FalconError::ParseError)?
            .into_iter()
            .peekable();

        self.populate_main_context(tokens)
            .map_err(FalconError::ParseError)?;

        codegen::build_context(self.asm_context).map_err(FalconError::ParseError)
    }
}

//! Error type that is used by the assembler.

use std::fmt;

use nom::Finish;

use crate::assembler::lexer::Token;
use crate::assembler::parser;
use crate::assembler::span::ParseSpan;

/// An error that occurred while parsing assembly.
///
/// Contains debug information about the position where the error occurred
/// and what the error itself was caused by.
#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    /// A tokenization error that indicates unparseable characters.
    Tokenization(ParseSpan<String>),
    /// A parser error that indicates valid, but out-of-place tokens in the source.
    UnexpectedToken(ParseSpan<String>),
    /// A code generation error that indicates a reference to a symbol that was
    /// never defined in code.
    UndefinedSymbol(ParseSpan<String>),
    /// A code generation error that indicates a reference to a symbol that has
    /// multiple non-unique definitions.
    RedefinedSymbol(ParseSpan<String>),
    /// A instruction selection error indicating that user-provided assembly does
    /// not match any of the supported forms of the instruction.
    InstructionSelection(ParseSpan<String>),
    /// A code generation error that indicates a failure to process a relocation,
    /// for example because of a missing symbol definition.
    Relocation(ParseSpan<String>),
}

impl ParseError {
    fn get_span(&self) -> &ParseSpan<String> {
        match self {
            ParseError::Tokenization(span) => span,
            ParseError::UnexpectedToken(span) => span,
            ParseError::UndefinedSymbol(span) => span,
            ParseError::RedefinedSymbol(span) => span,
            ParseError::InstructionSelection(span) => span,
            ParseError::Relocation(span) => span,
        }
    }

    /// Gets the reason behind the error.
    pub fn reason(&self) -> &str {
        self.get_span().token()
    }

    /// Gets the filename where the error came from.
    pub fn file(&self) -> &str {
        self.get_span().file.to_str().unwrap_or("<unknown>")
    }

    /// Gets the line number at which the error occurred.
    pub fn line_number(&self) -> usize {
        self.get_span().lineno
    }

    /// Gets the contents of the line where the error occurred.
    pub fn line(&self) -> &str {
        &self.get_span().line
    }

    /// Gets the position within the line at which the error occurred.
    pub fn position(&self) -> usize {
        self.get_span().offset
    }

    /// Gets the width of the error span.
    pub fn width(&self) -> usize {
        self.get_span().width
    }

    // Consumes nom's IResult of the tokenization step and checks for potential errors.
    pub(crate) fn check_tokenization<'a>(
        result: nom::IResult<parser::LineSpan<'a>, Vec<ParseSpan<Token<'a>>>>,
    ) -> Result<Vec<ParseSpan<Token<'a>>>, Self> {
        match result.finish() {
            Ok((_, tokens)) => Ok(tokens),
            Err(e) => {
                let span = e.input;

                // If we could actually parse the remaining input into a valid token,
                // it usually means that a missing whitespace was causing the error.
                let mut extra = "";
                if let Ok(_) = Token::from_span(span) {
                    extra = " Maybe insert a whitespace?";
                }

                Err(ParseError::Tokenization(ParseSpan::new(
                    span,
                    span.chars().take_while(|c| !c.is_whitespace()).count(),
                    format!("Unparseable tokens detected.{}", extra),
                )))
            }
        }
    }

    // Consumes the span of a token that failed to be turned into a valid instruction
    // form during instruction selection.
    pub(crate) fn build_tokenization_error<'a>(span: ParseSpan<Token<'a>>) -> Self {
        ParseError::Tokenization(ParseSpan {
            file: span.file,
            line: span.line,
            lineno: span.lineno,
            offset: span.offset,
            width: span.width,
            token: "Unparseable tokens detected.".to_owned(),
        })
    }

    // Consumes the span of a symbol that could not be processed by the code
    // generator as it is out of place.
    pub(crate) fn build_unexpected_token_error<'a>(span: ParseSpan<Token<'a>>) -> Self {
        ParseError::UnexpectedToken(ParseSpan {
            file: span.file,
            line: span.line,
            lineno: span.lineno,
            offset: span.offset,
            width: span.width,
            token: "Unexpected token encountered.".to_owned(),
        })
    }

    // Consumes the span of a symbol that could not be resolved due to missing
    // definition.
    pub(crate) fn build_undefined_symbol_error<'a>(span: ParseSpan<Token<'a>>) -> Self {
        ParseError::UndefinedSymbol(ParseSpan {
            file: span.file,
            line: span.line,
            lineno: span.lineno,
            offset: span.offset,
            width: span.width,
            token: "Symbol used, but never defined.".to_owned(),
        })
    }

    // Consumes the span of a symbol that could not be resolved due to multiple
    // definitions.
    pub(crate) fn build_redefined_symbol_error<'a>(span: ParseSpan<Token<'a>>) -> Self {
        ParseError::RedefinedSymbol(ParseSpan {
            file: span.file,
            line: span.line,
            lineno: span.lineno,
            offset: span.offset,
            width: span.width,
            token: "Mutliple definitions found for symbol.".to_owned(),
        })
    }

    // Consumes the span of an mnemonic that failed to be turned into a valid
    // instruction form during instruction selection.
    pub(crate) fn build_instruction_selection_error<'a>(span: ParseSpan<Token<'a>>) -> Self {
        ParseError::InstructionSelection(ParseSpan {
            file: span.file,
            line: span.line,
            lineno: span.lineno,
            offset: span.offset,
            width: span.width,
            token: "Failed to select a matching instruction form.".to_owned(),
        })
    }

    // Consumes the span of a symbol that triggered a relocation which could not
    // be resolved into a valid destination.
    pub(crate) fn build_relocation_error<'a>(span: ParseSpan<Token<'a>>) -> Self {
        ParseError::Relocation(ParseSpan {
            file: span.file,
            line: span.line,
            lineno: span.lineno,
            offset: span.offset,
            width: span.width,
            token: "Failed to process the relocation.".to_owned(),
        })
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error: {reason}\n{arrow:>6} {file}:{lineno}:{rpos}\n\n{lpadc:>lpad$}{line}\n{mark:>pos$}",
            reason = self.reason(),
            arrow = "-->",
            file = self.file(),
            lineno = self.line_number(),
            rpos = self.position(),
            lpadc = " ",
            lpad = 10,
            line = self.line(),
            mark = "^".repeat(self.width()),
            pos = 9 + self.position() + self.width(),
        )
    }
}

impl std::error::Error for ParseError {}

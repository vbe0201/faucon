//! Error type that is used by the assembler.

use std::error::Error;
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
    /// The input source to parse is not encoded as valid UTF-8.
    Encoding,
    /// A tokenization error that indicates unparseable characters.
    ///
    /// The [`ParseSpan`] it encapsulates wraps up the faulting line information
    /// along with a descriptive error message at its heart.
    Tokenization(ParseSpan<String>),
}

impl ParseError {
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
                    1,
                    format!("Unparseable tokens detected.{}", extra),
                )))
            }
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl Error for ParseError {}

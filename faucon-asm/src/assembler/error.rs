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
    ///
    /// The [`ParseSpan`] it encapsulates wraps up the faulting line information
    /// along with a descriptive error message at its heart.
    Tokenization(ParseSpan<String>),
}

impl ParseError {
    /// Gets the reason behind the error.
    pub fn reason(&self) -> &str {
        match self {
            ParseError::Tokenization(span) => span.token(),
        }
    }

    /// Gets the filename where the error came from.
    pub fn file(&self) -> &str {
        match self {
            ParseError::Tokenization(span) => span.file.to_str().unwrap_or("<unknown>"),
        }
    }

    /// Gets the line number at which the error occurred.
    pub fn line_number(&self) -> usize {
        match self {
            ParseError::Tokenization(span) => span.lineno,
        }
    }

    /// Gets the contents of the line where the error occurred.
    pub fn line(&self) -> &str {
        match self {
            ParseError::Tokenization(span) => &span.line,
        }
    }

    /// Gets the position within the line at which the error occurred.
    pub fn position(&self) -> usize {
        match self {
            ParseError::Tokenization(span) => span.offset,
        }
    }

    /// Gets the width of the error span.
    pub fn width(&self) -> usize {
        match self {
            ParseError::Tokenization(span) => span.width,
        }
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

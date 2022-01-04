use std::fmt;

use nom::Finish;
use owo_colors::OwoColorize;

use super::{
    parser,
    span::{Span, Spanned},
};

/// An error that occurred at one stage of the assembling process.
///
/// Contains debug and span information about the token and what the
/// error was caused by.
#[derive(Debug)]
pub struct AssemblerError {
    span: Span,
    quoted: (String, bool), // Quoted string + whether it is a fragment of a full line.
    msg: String,
}

impl AssemblerError {
    pub(crate) fn new<S: ToString>(nom_span: parser::NomSpan<'_>, msg: S) -> Self {
        let span = Span::from_nom(
            &nom_span,
            nom_span
                .chars()
                .take_while(|&c| !c.is_whitespace() && c != ';')
                .count(),
        );
        let quoted = nom_span
            .extra
            .extract_line(nom_span.location_offset())
            .to_owned();

        Self {
            span,
            quoted: (quoted, false),
            msg: msg.to_string(),
        }
    }

    pub(crate) fn custom<S: ToString, T>(input: &str, span: Spanned<T>, msg: S) -> Self {
        let span = span.into_span();
        let quoted = format!(
            "{dots}{line}{dots}",
            dots = "...".blue(),
            line = &input[&span]
        );

        Self {
            span,
            quoted: (quoted, true),
            msg: msg.to_string(),
        }
    }

    /// Gets the span of the erroneous assembler source.
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// Gets the quoted string that caused the error.
    pub fn quoted(&self) -> &str {
        &self.quoted.0
    }

    /// Gets the message of this error which provides further details.
    pub fn message(&self) -> &str {
        &self.msg
    }

    // Consumes nom's IResult from the tokenization step and checks for errors.
    pub(crate) fn check_tokenization<'t>(
        result: nom::IResult<parser::NomSpan<'t>, Vec<parser::Statement<'t>>>,
    ) -> Result<Vec<parser::Statement<'t>>, Self> {
        match result.finish() {
            Ok((_, tokens)) => Ok(tokens),
            Err(e) => Err(Self::new(e.input, "Unparseable tokens detected")),
        }
    }
}

impl fmt::Display for AssemblerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "at line {}, column {}: {}",
            self.span().line(),
            self.span().column(),
            self.msg
        )
    }
}

impl std::error::Error for AssemblerError {}

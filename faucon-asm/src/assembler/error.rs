use nom::Finish;

use super::{
    lexer::Token,
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
    line: String,
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
        let line = nom_span
            .extra
            .extract_line(nom_span.location_offset())
            .to_owned();

        Self {
            span,
            line,
            msg: msg.to_string(),
        }
    }

    /// Gets the span of the erroneous assembler source.
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// Gets the source line in which the error occurred.
    pub fn line(&self) -> &str {
        &self.line
    }

    /// Gets the message of this error which provides further details.
    pub fn message(&self) -> &str {
        &self.msg
    }

    // Consumes nom's IResult from the tokenization step and checks for errors.
    pub(crate) fn check_tokenization<'t>(
        result: nom::IResult<parser::NomSpan<'t>, Vec<Spanned<Token<'t>>>>,
    ) -> Result<Vec<Spanned<Token<'t>>>, Self> {
        match result.finish() {
            Ok((_, tokens)) => Ok(tokens),
            Err(e) => Err(Self::new(e.input, "Unparseable tokens detected")),
        }
    }
}

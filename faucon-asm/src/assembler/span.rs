//! A span type implementation providing line and position information for parsed
//! tokens and error messages.

use std::str::from_utf8;

use super::parser::LineSpan;

/// Matches an object from the given parser and wraps it in a [`ParseSpan`].
pub fn spanned<'a, T>(
    mut parser: impl FnMut(LineSpan<'a>) -> nom::IResult<LineSpan<'a>, T>,
) -> impl FnMut(LineSpan<'a>) -> nom::IResult<LineSpan<'a>, ParseSpan<T>> {
    move |s: LineSpan<'a>| {
        let (input, position) = nom_locate::position(s)?;
        let (remainder, token) = parser(input)?;

        Ok((
            remainder,
            ParseSpan::new(
                position,
                input.fragment().len() - remainder.fragment().len(),
                token,
            ),
        ))
    }
}

/// Wraps a parsed token along with information about its encoding in the
/// input source.
#[derive(Debug, PartialEq, Eq)]
pub struct ParseSpan<T> {
    // The contents of the line in which the spanned source element is.
    line: String,
    // The line number in which the spanned source element is. Since the
    // line contents and the position of the token are already known, this
    // is mostly intended for debugging and formatting purposes.
    lineno: usize,
    // The offset into the source string where the spanned token begins.
    offset: usize,
    // The width of the spanned token within the input source string. This
    // could also be referred to as the number of characters encapsulating
    // the source element.
    width: usize,
    // The spanned token object that has been parsed out of the input source.
    token: T,
}

impl<T> ParseSpan<T> {
    /// Constructs a new [`ParseSpan`] from the line span denoting the location of
    /// the token, the encoded width of the token and the token itself.
    ///
    /// This should never be called manually, refer to [`spanned`] instead.
    pub fn new<'a>(location_span: LineSpan<'a>, width: usize, token: T) -> Self {
        ParseSpan {
            line: from_utf8(location_span.get_line_beginning())
                .unwrap_or("[cannot display line]")
                .to_string(),
            lineno: location_span.location_line() as usize,
            offset: location_span.naive_get_utf8_column(),
            width,
            token,
        }
    }

    /// Unwraps the `ParseSpan` into the encapsulated token object.
    pub fn unwrap(self) -> T {
        self.token
    }
}

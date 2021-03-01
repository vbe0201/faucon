use super::parser::{LineSpan, ParserResult};

// Matches an object from the given parser and wraps it in a [`ParseSpan`].
pub fn spanned<'a, T>(
    mut parser: impl FnMut(LineSpan<'a>) -> ParserResult<'a, T>,
) -> impl FnMut(LineSpan<'a>) -> ParserResult<'a, ParseSpan<T>> {
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

// Wraps a parsed token along with information about its encoding in the
// input source.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParseSpan<T> {
    // The line in which the spanned source element is. Since the `offset`
    // field already suffices to determine the position, this is mostly
    // intended to be used for error formatting and debugging purposes.
    line: usize,
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
    // Constructs a new `ParseSpan` from the line span denoting the location of
    // the token, the encoded width of the token and the token itself.
    //
    // This should never be called manually, refer to [`spanned`] instead.
    pub fn new<'a>(location_span: LineSpan<'a>, width: usize, token: T) -> Self {
        ParseSpan {
            line: location_span.location_line() as usize,
            offset: location_span.location_offset(),
            width,
            token,
        }
    }

    // Unwraps the `ParseSpan` into the encapsulated token object.
    pub fn unwrap(self) -> T {
        self.token
    }
}

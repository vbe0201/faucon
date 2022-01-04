use std::fmt;
use std::ops::{Index, Range};

use super::{interner::FileId, parser::NomSpan};

#[derive(Clone, Copy, PartialEq)]
pub struct Span {
    file_id: FileId,
    line: u32,
    column: usize,
    start: usize,
    end: usize,
}

impl Span {
    pub const DUMMY: Span = Span {
        file_id: FileId::DUMMY,
        line: 0,
        column: 0,
        start: 0,
        end: 0,
    };

    pub fn new<ID: Into<FileId>>(
        file_id: ID,
        line: u32,
        column: usize,
        start: usize,
        end: usize,
    ) -> Self {
        Self {
            file_id: file_id.into(),
            line,
            column,
            start,
            end,
        }
    }

    pub fn from_nom(span: &NomSpan<'_>, width: usize) -> Self {
        let start = span.location_offset();
        Self {
            file_id: span.extra.file_id,
            line: span.location_line(),
            column: span.naive_get_utf8_column(),
            start,
            end: start + width,
        }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn width(&self) -> usize {
        self.end - self.start
    }

    pub fn as_range(&self) -> Range<usize> {
        self.start..self.end
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Span")
            .field(&format_args!("{}..{}", self.start, self.end))
            .finish()
    }
}

impl Index<&Span> for str {
    type Output = str;

    fn index(&self, span: &Span) -> &Self::Output {
        self.index(span.start..span.end)
    }
}

impl From<Span> for FileId {
    fn from(span: Span) -> Self {
        span.file_id
    }
}

#[derive(PartialEq)]
pub struct Spanned<T> {
    node: T,
    span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Spanned { node, span }
    }

    pub fn boxed(node: T, span: Span) -> Spanned<Box<T>> {
        Spanned {
            node: Box::new(node),
            span,
        }
    }

    pub fn try_map<F, U>(self, f: F) -> Result<Spanned<U>, Spanned<T>>
    where
        F: FnOnce(T) -> Result<U, T>,
    {
        let Self { node, span } = self;

        f(node)
            .map(|node| Spanned { node, span })
            .map_err(|node| Spanned { node, span })
    }

    pub fn parse<'a>(
        mut parser: impl FnMut(NomSpan<'a>) -> nom::IResult<NomSpan<'a>, T>,
    ) -> impl FnMut(NomSpan<'a>) -> nom::IResult<NomSpan<'a>, Spanned<T>> {
        move |s: NomSpan<'a>| {
            let (input, position) = nom_locate::position(s)?;
            let input_len = input.fragment().len();

            let (remainder, token) = parser(input)?;
            let span_width = input_len - remainder.fragment().len();

            Ok((
                remainder,
                Self::new(token, Span::from_nom(&position, span_width)),
            ))
        }
    }

    pub fn node(&self) -> &T {
        &self.node
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn into_node(self) -> T {
        self.node
    }

    pub fn into_span(self) -> Span {
        self.span
    }
}

impl<T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        Self {
            node: self.node.clone(),
            span: self.span,
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Spanned")
            .field("node", &self.node)
            .field("span", &self.span)
            .finish()
    }
}

impl<T> From<Spanned<T>> for FileId {
    fn from(spanned: Spanned<T>) -> Self {
        spanned.span().file_id
    }
}

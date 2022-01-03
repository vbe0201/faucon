use std::fmt;
use std::ops::{Index, Range};

use super::{interner::FileId, parser::NomSpan};

#[derive(Clone, Copy, PartialEq)]
pub struct Span {
    file_id: FileId,
    line: u32,
    start_index: usize,
    end_index: usize,
}

impl Span {
    pub const DUMMY: Span = Span {
        file_id: FileId::DUMMY,
        line: 0,
        start_index: 0,
        end_index: 0,
    };

    pub fn new<ID: Into<FileId>>(
        file_id: ID,
        line: u32,
        start_index: usize,
        end_index: usize,
    ) -> Self {
        Span {
            file_id: file_id.into(),
            line,
            start_index,
            end_index,
        }
    }

    pub fn from_nom(span: &NomSpan<'_>, width: usize) -> Self {
        let start_index = span.naive_get_utf8_column();
        Self {
            file_id: span.extra.file_id,
            line: span.location_line(),
            start_index,
            end_index: start_index + width,
        }
    }

    pub fn start(&self) -> usize {
        self.start_index
    }

    pub fn end(&self) -> usize {
        self.end_index
    }

    pub fn width(&self) -> usize {
        self.end_index - self.start_index
    }

    pub fn as_range(&self) -> Range<usize> {
        self.start_index..self.end_index
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Span")
            .field(&format_args!("{}..{}", self.start_index, self.end_index))
            .finish()
    }
}

impl Index<Span> for str {
    type Output = str;

    fn index(&self, span: Span) -> &Self::Output {
        self.index(span.start_index..span.end_index)
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
}

impl<T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        Self {
            node: self.node.clone(),
            span: self.span.clone(),
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

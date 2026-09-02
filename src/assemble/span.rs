use std::ops::{Deref, DerefMut, Index, Range};

/// Describes a byte offset into a piece of source code.
///
/// Note that [`SourceLocation`]s are not attributed to the
/// source string they reference, which makes it the user's
/// responsibility to track these logical relations.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SourceLocation(u32);

impl SourceLocation {
    /// Constructs a new source location from a byte offset.
    pub const fn new(pos: u32) -> Self {
        Self(pos)
    }

    /// Converts this source location to 1-based line and column.
    pub fn as_line_and_column(self, input: &str) -> (u32, u32) {
        let mut line = 1;
        let mut column = 1;

        let mut pos = 0;
        for c in input.chars() {
            if pos >= self.0 {
                break;
            }

            if c == '\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }

            pos += c.len_utf8() as u32;
        }

        (line, column)
    }
}

/// A span of text in a piece of source code.
///
/// This is used to attribute tokens with their textual form.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SourceSpan {
    start: SourceLocation,
    end: SourceLocation,
}

impl SourceSpan {
    /// Creates a new source span from two byte offsets.
    pub const fn new(start: u32, end: u32) -> Self {
        debug_assert!(start <= end);
        Self {
            start: SourceLocation::new(start),
            end: SourceLocation::new(end),
        }
    }

    /// Gets the start offset (inclusive) of the span.
    pub fn start(self) -> SourceLocation {
        self.start
    }

    /// Gets the end offset (exclusive) of the span.
    pub fn end(self) -> SourceLocation {
        self.end
    }

    /// Gets the byte length of the span.
    pub fn len(self) -> u32 {
        self.end.0 - self.start.0
    }

    /// Attempts to extract a spanned substring from `input`.
    ///
    /// Returns [`None`] if the span is out of bounds for the
    /// given string, or does not span valid UTF-8 text.
    pub fn text(self, input: &str) -> Option<&str> {
        let range: Range<usize> = self.into();
        input.get(range)
    }
}

impl From<SourceSpan> for Range<u32> {
    fn from(value: SourceSpan) -> Self {
        value.start.0..value.end.0
    }
}

impl From<SourceSpan> for Range<usize> {
    fn from(value: SourceSpan) -> Self {
        value.start.0 as usize..value.end.0 as usize
    }
}

impl From<Range<u32>> for SourceSpan {
    fn from(value: Range<u32>) -> Self {
        Self::new(value.start, value.end)
    }
}

impl Index<SourceSpan> for str {
    type Output = str;

    fn index(&self, index: SourceSpan) -> &Self::Output {
        let range: Range<usize> = index.into();
        &self[range]
    }
}

/// A container that attributes a value with a [`SourceSpan`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Spanned<T> {
    /// The associated span.
    pub span: SourceSpan,
    /// The wrapped value.
    pub value: T,
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

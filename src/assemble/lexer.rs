use std::str::CharIndices;

use crate::assemble::span::{SourceSpan, Spanned};

const EOF_CHAR: char = '\0';

/// Represents a lexeme of the Falcon assembler language.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token {
    /// `[`
    LeftBracket,
    /// `]`
    RightBracket,

    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `.`
    Dot,
    /// `:`
    Colon,
    /// `,`
    Comma,
    /// `;`
    Semicolon,
    /// `#`
    Hash,
    /// `$`
    Dollar,

    /// An identifier.
    Identifier,
    /// A string literal.
    String,
    /// A number literal.
    Number,

    /// An error occurred during lexing.
    Error,
    /// End of input was reached.
    Eof,
}

#[inline]
fn is_ident1(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

#[inline]
fn is_ident2(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

/// Breaks down a piece of source code into [`Token`]s.
///
/// Lexing is the first stage of the assembler and breaks
/// a given program down into a sequence of tokens:
///
/// ```no_run
/// mov $r10, 0xDEADBEEF
/// ```
///
/// is turned into the following stream:
///
/// ```no_run
/// Identifier Dollar Identifier Comma Number Semicolon
/// ```
///
/// These tokens are then processed in subsequent stages
/// of the assembler. Implicit semicolons are inserted at
/// every newline character and EOF to terminate statements.
///
/// The lexer is driven by its [`Iterator`] interface which
/// stops after [`Token::Eof`] has been produced once.
#[derive(Clone, Debug)]
pub struct Lexer<'src> {
    source: CharIndices<'src>,
}

impl<'src> Lexer<'src> {
    /// Creates a lexer for a given assembler program.
    pub fn new(source: &'src str) -> Self {
        debug_assert!(u32::try_from(source.len()).is_ok());

        Self {
            source: source.char_indices(),
        }
    }

    fn offset(&self) -> u32 {
        self.source.offset() as u32
    }

    fn peek(&self) -> char {
        let mut it = self.source.clone();
        it.next().map(|v| v.1).unwrap_or(EOF_CHAR)
    }

    fn peek2(&self) -> char {
        let mut it = self.source.clone();
        it.next().and(it.next()).map(|v| v.1).unwrap_or(EOF_CHAR)
    }

    fn reached_eof(&self) -> bool {
        self.peek() == EOF_CHAR
    }

    fn consume(&mut self) -> char {
        self.source.next().map(|v| v.1).unwrap_or(EOF_CHAR)
    }

    fn line_comment(&mut self) {
        while self.peek() != '\n' && !self.reached_eof() {
            self.consume();
        }
    }

    fn multi_line_comment(&mut self) {
        self.consume();
        self.consume();

        while !(self.peek() == '*' && self.peek2() == '/') {
            if self.reached_eof() {
                return;
            }

            self.consume();
        }

        self.consume();
        self.consume();
    }

    fn whitespace(&mut self) -> Option<Spanned<Token>> {
        let mut token = None;

        loop {
            match self.peek() {
                // Handle semicolons and turn newlines into semicolons as
                // well. Multiple semicolons in a whitespace sequence are
                // coalesced into a single semicolon token.
                ';' | '\n' => {
                    let pos = self.offset();
                    token = Some(Spanned {
                        span: SourceSpan::from(pos..pos),
                        value: Token::Semicolon,
                    });

                    self.consume();
                }

                // Other whitespace can be trivially ignored.
                c if c.is_whitespace() => {
                    self.consume();
                }

                // Treat comments as whitespace too.
                '/' => {
                    let c2 = self.peek2();
                    if c2 == '/' {
                        self.line_comment();
                    } else if c2 == '*' {
                        self.multi_line_comment();
                    } else {
                        break;
                    }
                }

                _ => break,
            }
        }

        token
    }

    fn string(&mut self) -> Token {
        while self.peek() != '"' {
            if self.reached_eof() {
                return Token::Error;
            }

            self.consume();
        }

        self.consume();
        Token::String
    }

    fn number(&mut self, first: char) -> Token {
        let radix = match (first, self.peek()) {
            ('0', 'x' | 'X') => {
                self.consume();
                self.consume();
                16
            }
            ('0', 'b' | 'B') => {
                self.consume();
                self.consume();
                2
            }
            _ => 10,
        };

        while self.peek().is_digit(radix) {
            self.consume();
        }

        Token::Number
    }

    fn name(&mut self) -> Token {
        while is_ident2(self.peek()) {
            self.consume();
        }

        Token::Identifier
    }

    fn scan(&mut self) -> Spanned<Token> {
        use Token::*;

        // First, consume whitespace and inject semicolons, if
        // necessary. A call consumes all available whitespace
        // so the next call is guaranteed to not return semicolon.
        if let Some(token) = self.whitespace() {
            return token;
        }

        let start = self.offset();

        // Consume the next non-whitespace character.
        let c = self.consume();
        if c == EOF_CHAR {
            return Spanned {
                span: SourceSpan::from(start..start),
                value: Token::Eof,
            };
        }

        let token = match c {
            c if is_ident1(c) => self.name(),
            c if c.is_ascii_digit() => self.number(c),
            '"' => self.string(),

            '[' => LeftBracket,
            ']' => RightBracket,
            '+' => Plus,
            '-' => Minus,
            '*' => Star,
            '.' => Dot,
            ':' => Colon,
            ',' => Comma,
            ';' => Semicolon,
            '#' => Hash,
            '$' => Dollar,

            _ => Error,
        };

        let end = self.offset();
        Spanned {
            span: SourceSpan::from(start..end),
            value: token,
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.scan();
        if token.value != Token::Eof {
            Some(token)
        } else {
            None
        }
    }
}

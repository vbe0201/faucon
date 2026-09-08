use std::{iter::Peekable, mem};

use crate::{
    assemble::{
        ast,
        diagnostic::Diagnostic,
        lexer::{Lexer, Token},
        span::{SourceSpan, Spanned},
    },
    isa,
};

/// Parses an assembler program into an Abstract Syntax Tree.
///
/// Parsing drives the [`Lexer`] and attempts to build high-level
/// syntactic structures from the rather primitive [`Token`]s
/// that comprise a program.
///
/// A program is represented by an [`ast::Program`] as a sequence
/// of [statements][ast::Statement]. If there is a syntax error
/// in the middle of a statement, the parser resynchronizes to
/// the next semicolon and emits a diagnostic.
#[derive(Clone, Debug)]
pub struct Parser<'src> {
    source: &'src str,
    lexer: Peekable<Lexer<'src>>,
    diagnostics: Vec<Diagnostic>,
}

impl<'src> Parser<'src> {
    /// Creates a new parser over a given input program.
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            lexer: Lexer::new(source).peekable(),
            diagnostics: Vec::new(),
        }
    }

    /// Attempts to parse the full input program into an AST.
    ///
    /// Note that an AST is only formed from syntactically correct
    /// programs. If one or more errors occur while parsing, a list
    /// of diagnostics is returned instead.
    pub fn parse(&mut self) -> Result<ast::Program, Vec<Diagnostic>> {
        let mut program = ast::Program {
            statements: Vec::new(),
        };

        while !self.eof() {
            match self.statement() {
                Some(stmt) => program.statements.push(stmt),
                None => self.synchronize(),
            }
        }

        if self.diagnostics.is_empty() {
            Ok(program)
        } else {
            Err(mem::take(&mut self.diagnostics))
        }
    }

    fn text(&self, span: SourceSpan) -> &'src str {
        &self.source[span]
    }

    fn peek(&mut self) -> Token {
        self.lexer.peek().map(|t| t.value).unwrap_or(Token::Eof)
    }

    fn peek_span(&mut self) -> SourceSpan {
        self.lexer.peek().map(|t| t.span).unwrap_or_else(|| {
            let pos = self.source.len() as u32;
            SourceSpan::from(pos..pos)
        })
    }

    fn at(&mut self, token: Token) -> bool {
        self.peek() == token
    }

    fn eof(&mut self) -> bool {
        self.at(Token::Eof)
    }

    fn consume(&mut self) -> Option<Spanned<Token>> {
        self.lexer.next()
    }

    fn error_at(&mut self, span: SourceSpan, message: impl ToString) {
        self.diagnostics.push(Diagnostic::error(span, message));
    }

    fn error(&mut self, message: impl ToString) {
        let span = self.peek_span();
        self.error_at(span, message);
    }

    fn eat(&mut self, token: Token) -> Option<Spanned<Token>> {
        self.lexer.next_if(|actual| actual.value == token)
    }

    fn synchronize(&mut self) {
        while !self.eof() {
            let prev = self.consume();

            // Once we consumed the next semicolon, we're done.
            if let Some(Spanned {
                value: Token::Semicolon,
                ..
            }) = prev
            {
                break;
            }
        }
    }

    fn statement(&mut self) -> Option<ast::Statement> {
        let stmt = match self.peek() {
            Token::Identifier => {
                let ident = self.consume().unwrap();
                if self.at(Token::Colon) {
                    self.consume();
                    ast::Statement::Label(ident.span)
                } else {
                    self.instruction(ident).map(ast::Statement::Instruction)?
                }
            }
            Token::Dot => self.directive().map(ast::Statement::Directive)?,
            _ => {
                self.error("expected start of statement");
                return None;
            }
        };

        if self.eat(Token::Semicolon).is_none() {
            self.error("expected end of statement");
            return None;
        }

        Some(stmt)
    }

    fn instruction(&mut self, mnemonic: Spanned<Token>) -> Option<ast::Instruction> {
        let Ok(opcode) = self.text(mnemonic.span).parse::<isa::Opcode>() else {
            self.error_at(mnemonic.span, "unknown mnemonic");
            return None;
        };

        let operand_size = self
            .eat(Token::Dot)
            .and_then(|_| {
                let Some(Spanned { span, .. }) = self.eat(Token::Identifier) else {
                    self.error("expected size suffix");
                    return None;
                };

                match self.text(span) {
                    "b" => Some(isa::OperandSize::EightBit),
                    "h" => Some(isa::OperandSize::SixteenBit),
                    "w" => Some(isa::OperandSize::ThirtyTwoBit),
                    _ => {
                        self.error_at(span, "unknown operand size");
                        None
                    }
                }
            })
            .unwrap_or(isa::OperandSize::Unsized);

        let operands = self.value_list()?;

        Some(ast::Instruction {
            mnemonic: opcode,
            size: operand_size,
            operands,
        })
    }

    fn directive(&mut self) -> Option<ast::Directive> {
        if self.eat(Token::Dot).is_none() {
            self.error("expected dot");
            return None;
        };

        let Some(ident) = self.eat(Token::Identifier) else {
            self.error("expected identifier");
            return None;
        };

        let operands = self.value_list()?;

        Some(ast::Directive {
            name: ident.span,
            operands,
        })
    }

    fn value_list(&mut self) -> Option<[Option<Spanned<ast::Value>>; 3]> {
        let mut res = [const { None }; 3];

        if !self.at(Token::Semicolon) {
            res[0] = Some(self.value()?);

            if self.eat(Token::Comma).is_some() {
                res[1] = Some(self.value()?);
            }

            if self.eat(Token::Comma).is_some() {
                res[2] = Some(self.value()?);
            }
        }

        Some(res)
    }

    fn value(&mut self) -> Option<Spanned<ast::Value>> {
        match self.peek() {
            Token::Plus | Token::Minus | Token::Number => {
                let imm = self.immediate()?;
                if self.at(Token::Colon) {
                    self.bits(imm).map(|v| Spanned {
                        span: v.span,
                        value: ast::Value::Bits(v.value),
                    })
                } else {
                    Some(Spanned {
                        span: imm.span,
                        value: ast::Value::Int(imm.value),
                    })
                }
            }

            Token::Hash => self.label().map(|v| Spanned {
                span: v.span,
                value: ast::Value::Label(v.value),
            }),

            Token::Dollar => self.register_or_flag(),

            Token::String => self.string().map(|v| Spanned {
                span: v.span,
                value: ast::Value::Str(v.value),
            }),

            Token::Identifier => self.memory_access().map(|v| Spanned {
                span: v.span,
                value: ast::Value::Mem(v.value),
            }),

            _ => {
                self.error("expected operand");
                None
            }
        }
    }

    fn label(&mut self) -> Option<Spanned<SourceSpan>> {
        let Some(hash) = self.eat(Token::Hash) else {
            self.error("expected #");
            return None;
        };

        let Some(ident) = self.eat(Token::Identifier) else {
            self.error("expected identifier");
            return None;
        };

        Some(Spanned {
            span: hash.span.merge(ident.span),
            value: ident.span,
        })
    }

    fn string(&mut self) -> Option<Spanned<SourceSpan>> {
        let Some(str) = self.eat(Token::String) else {
            self.error("expected string literal");
            return None;
        };

        Some(Spanned {
            span: str.span,
            value: str.span.trim_start(1).trim_end(1),
        })
    }

    fn immediate(&mut self) -> Option<Spanned<isa::Immediate>> {
        let span = self.peek_span();

        let negative = if self.at(Token::Plus) {
            self.consume();
            false
        } else if self.at(Token::Minus) {
            self.consume();
            true
        } else {
            false
        };

        let Some(number) = self.eat(Token::Number) else {
            self.error("expected integer literal");
            return None;
        };

        let span = span.merge(number.span);

        let number_str = self.text(number.span);
        let (radix, digits) = match number_str.get(..2) {
            Some(s) if s.eq_ignore_ascii_case("0b") => (2, &number_str[2..]),
            Some(s) if s.eq_ignore_ascii_case("0x") => (16, &number_str[2..]),
            _ => (10, number_str),
        };

        let mut value = match u32::from_str_radix(digits, radix) {
            Ok(v) => v,
            Err(e) => {
                self.error_at(number.span, e);
                return None;
            }
        };

        if negative {
            let Ok(signed) = i32::try_from(value) else {
                self.error_at(number.span, "value out of signed range");
                return None;
            };

            value = (-signed).cast_unsigned();
        }

        Some(Spanned {
            span,
            value: isa::Immediate {
                value,
                signed: negative,
            },
        })
    }

    fn register_or_flag(&mut self) -> Option<Spanned<ast::Value>> {
        let Some(dollar) = self.eat(Token::Dollar) else {
            self.error("expected $");
            return None;
        };

        let Some(ident) = self.eat(Token::Identifier) else {
            self.error("expected register or flag literal");
            return None;
        };

        let value = match self.text(ident.span) {
            "r0" => ast::Value::Reg(isa::Register::REG0),
            "r1" => ast::Value::Reg(isa::Register::REG1),
            "r2" => ast::Value::Reg(isa::Register::REG2),
            "r3" => ast::Value::Reg(isa::Register::REG3),
            "r4" => ast::Value::Reg(isa::Register::REG4),
            "r5" => ast::Value::Reg(isa::Register::REG5),
            "r6" => ast::Value::Reg(isa::Register::REG6),
            "r7" => ast::Value::Reg(isa::Register::REG7),
            "r8" => ast::Value::Reg(isa::Register::REG8),
            "r9" => ast::Value::Reg(isa::Register::REG9),
            "r10" => ast::Value::Reg(isa::Register::REG10),
            "r11" => ast::Value::Reg(isa::Register::REG11),
            "r12" => ast::Value::Reg(isa::Register::REG12),
            "r13" => ast::Value::Reg(isa::Register::REG13),
            "r14" => ast::Value::Reg(isa::Register::REG14),
            "r15" => ast::Value::Reg(isa::Register::REG15),

            "iv0" => ast::Value::Reg(isa::Register::IV0),
            "iv1" => ast::Value::Reg(isa::Register::IV1),
            "iv2" => ast::Value::Reg(isa::Register::IV2),
            "ev" => ast::Value::Reg(isa::Register::EV),
            "sp" => ast::Value::Reg(isa::Register::SP),
            "pc" => ast::Value::Reg(isa::Register::PC),
            "imb" => ast::Value::Reg(isa::Register::IMB),
            "dmb" => ast::Value::Reg(isa::Register::DMB),
            "csw" => ast::Value::Reg(isa::Register::CSW),
            "ccr" => ast::Value::Reg(isa::Register::CCR),
            "sec" => ast::Value::Reg(isa::Register::SEC),
            "ctx" => ast::Value::Reg(isa::Register::CTX),
            "exci" => ast::Value::Reg(isa::Register::EXCI),
            "sec1" => ast::Value::Reg(isa::Register::SEC1),
            "imb1" => ast::Value::Reg(isa::Register::IMB1),
            "dmb1" => ast::Value::Reg(isa::Register::DMB1),

            "p0" => ast::Value::Flag(isa::Flag::P0),
            "p1" => ast::Value::Flag(isa::Flag::P1),
            "p2" => ast::Value::Flag(isa::Flag::P2),
            "p3" => ast::Value::Flag(isa::Flag::P3),
            "p4" => ast::Value::Flag(isa::Flag::P4),
            "p5" => ast::Value::Flag(isa::Flag::P5),
            "p6" => ast::Value::Flag(isa::Flag::P6),
            "p7" => ast::Value::Flag(isa::Flag::P7),
            "c" => ast::Value::Flag(isa::Flag::C),
            "o" => ast::Value::Flag(isa::Flag::O),
            "s" => ast::Value::Flag(isa::Flag::S),
            "z" => ast::Value::Flag(isa::Flag::Z),
            "ie0" => ast::Value::Flag(isa::Flag::IE0),
            "ie1" => ast::Value::Flag(isa::Flag::IE1),
            "ie2" => ast::Value::Flag(isa::Flag::IE2),
            "is0" => ast::Value::Flag(isa::Flag::IS0),
            "is1" => ast::Value::Flag(isa::Flag::IS1),
            "is2" => ast::Value::Flag(isa::Flag::IS2),
            "ea" => ast::Value::Flag(isa::Flag::EA),

            _ => {
                self.error_at(ident.span, "unknown flag or register");
                return None;
            }
        };

        Some(Spanned {
            span: dollar.span.merge(ident.span),
            value,
        })
    }

    fn register(&mut self) -> Option<Spanned<isa::Register>> {
        let Spanned { span, value } = self.register_or_flag()?;
        match value {
            ast::Value::Reg(reg) => Some(Spanned { span, value: reg }),
            _ => {
                self.error_at(span, "expected register, not flag");
                return None;
            }
        }
    }

    fn memory_offset(&mut self, span: SourceSpan) -> Option<Spanned<isa::MemoryOffset>> {
        if !self.at(Token::Plus) {
            return Some(Spanned {
                span,
                value: isa::MemoryOffset::None,
            });
        }

        let plus = self.consume()?;

        match self.peek() {
            Token::Plus | Token::Minus | Token::Number => {
                let imm = self.immediate()?;
                if imm.value.signed {
                    self.error_at(imm.span, "displacements cannot be negative");
                    return None;
                }

                Some(Spanned {
                    span: plus.span.merge(imm.span),
                    value: isa::MemoryOffset::Imm(imm.value.value),
                })
            }

            Token::Dollar => {
                let index = self.register()?;
                match self.eat(Token::Star) {
                    Some(star) => {
                        let scale = self.immediate()?;
                        if scale.value.signed {
                            self.error_at(scale.span, "scale cannot be negative");
                            return None;
                        }

                        let Ok(scale_value) = u8::try_from(scale.value.value) else {
                            self.error_at(scale.span, "scale must be an 8-bit immediate");
                            return None;
                        };

                        Some(Spanned {
                            span: index.span.merge(star.span).merge(scale.span),
                            value: isa::MemoryOffset::Reg {
                                index: index.value,
                                scale: scale_value,
                            },
                        })
                    }

                    None => Some(Spanned {
                        span: index.span,
                        value: isa::MemoryOffset::Reg {
                            index: index.value,
                            scale: 1,
                        },
                    }),
                }
            }

            _ => {
                self.error("expected register or integer literal");
                None
            }
        }
    }

    fn memory_access(&mut self) -> Option<Spanned<isa::MemoryAccess>> {
        let Some(space) = self.eat(Token::Identifier) else {
            self.error("expected identifier");
            return None;
        };

        let mem_space = match self.text(space.span) {
            "I" => isa::MemorySpace::Io,
            "D" => isa::MemorySpace::Data,
            _ => {
                self.error_at(space.span, "expected valid memory space");
                return None;
            }
        };

        let Some(lbracket) = self.eat(Token::LeftBracket) else {
            self.error("expected [");
            return None;
        };

        let base = self.register()?;
        let offset = self.memory_offset(base.span)?;

        let Some(rbracket) = self.eat(Token::RightBracket) else {
            self.error("expected ]");
            return None;
        };

        Some(Spanned {
            span: space
                .span
                .merge(lbracket.span)
                .merge(base.span)
                .merge(offset.span)
                .merge(rbracket.span),
            value: isa::MemoryAccess {
                space: mem_space,
                base: base.value,
                offset: offset.value,
            },
        })
    }

    fn bits(&mut self, start: Spanned<isa::Immediate>) -> Option<Spanned<isa::BitRange>> {
        let Some(colon) = self.eat(Token::Colon) else {
            self.error("expected :");
            return None;
        };

        let end = self.immediate()?;

        if start.value.signed {
            self.error_at(start.span, "bit index cannot be negative");
            return None;
        }

        if end.value.signed {
            self.error_at(end.span, "bit index cannot be negative");
            return None;
        }

        let Ok(start_value) = u8::try_from(start.value.value) else {
            self.error_at(start.span, "bit index exceeds value range");
            return None;
        };

        let Ok(end_value) = u8::try_from(end.value.value) else {
            self.error_at(end.span, "bit index exceeds value range");
            return None;
        };

        Some(Spanned {
            span: start.span.merge(colon.span).merge(end.span),
            value: isa::BitRange {
                start: start_value,
                end: end_value,
            },
        })
    }
}

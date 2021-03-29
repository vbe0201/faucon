use std::collections::BTreeMap;
use std::ffi::OsString;
use std::iter::Peekable;

use num_traits::{cast, NumCast};

use crate::assembler::lexer::Token;
use crate::assembler::span::ParseSpan;

// Assembler directives that may be used in Falcon assembly language.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Directive<'a> {
    // Specifies alignment of code to a given byte boundary.
    Align(u32),
    // Inserts a byte literal at the current position in code.
    Byte(u8),
    // Assigns a numeric value to a symbol with the supplied name.
    Equ(&'a str, u32),
    // Inserts a halfword literal at the current position in code in
    // Little-Endian byte order.
    Halfword(u16),
    // Includes another assembler source file relative to the file in
    // which the directive was used.
    Include(&'a str),
    // Inserts a word literal at the current position in code in
    // Little-Endian byte order.
    Word(u32),
    // Declares a code section with a name and an optional start address.
    Section(SecurityMode, &'a str, Option<u32>),
    // Skips the given amount of bytes in code and optionally fills them
    // with the supplied value.
    Skip(u32, Option<u8>),
    // Inserts a string literal at the current position in code.
    Str(&'a str),
}

#[inline]
fn try_extract_integer<'a, I: NumCast>(
    span: ParseSpan<Token<'a>>,
) -> Result<I, ParseSpan<Token<'a>>> {
    match span.token() {
        Token::SignedInt(i) => cast::<i32, I>(*i).ok_or(span),
        Token::UnsignedInt(i) => cast::<u32, I>(*i).ok_or(span),
        _ => Err(span),
    }
}

#[inline]
fn try_extract_integer_optional<'a, I: NumCast>(span: &ParseSpan<Token<'a>>) -> Option<I> {
    match span.token() {
        Token::SignedInt(i) => cast::<i32, I>(*i),
        Token::UnsignedInt(i) => cast::<u32, I>(*i),
        _ => None,
    }
}

#[inline]
fn try_extract_expression_ident<'a>(
    span: ParseSpan<Token<'a>>,
) -> Result<&'a str, ParseSpan<Token<'a>>> {
    match span.token() {
        Token::Expression(e) => Ok(e),
        _ => Err(span),
    }
}

#[inline]
fn try_extract_string<'a>(span: ParseSpan<Token<'a>>) -> Result<&'a str, ParseSpan<Token<'a>>> {
    match span.token() {
        Token::String(s) => Ok(s),
        _ => Err(span),
    }
}

macro_rules! parse_next_token {
    (int: $iter:ident => $int:ty) => {
        $iter
            .next()
            .ok_or(None)
            .and_then(|s| try_extract_integer::<$int>(s).map_err(Some))
    };
    (optint: $iter:ident => $int:ty) => {
        if let Some(v) = $iter
            .peek()
            .and_then(|s| try_extract_integer_optional::<$int>(s))
        {
            $iter.next();
            Some(v)
        } else {
            None
        }
    };
    (expr: $iter:ident) => {
        $iter
            .next()
            .ok_or(None)
            .and_then(|s| try_extract_expression_ident(s).map_err(Some))
    };
    (str: $iter:ident) => {
        $iter
            .next()
            .ok_or(None)
            .and_then(|s| try_extract_string(s).map_err(Some))
    };
}

// Consumes enough tokens from the supplied iterator to parse the `dir`
// directive.
//
// On failure to parse a required token to parse the full directive,
// said token is returned as the error. If however the given `dir`
// itself is no valid directive, `None` will be returned as the error.
pub fn parse_directive<'a, I: Iterator<Item = ParseSpan<Token<'a>>>>(
    dir: &'a str,
    iter: &mut Peekable<I>,
) -> Result<Directive<'a>, Option<ParseSpan<Token<'a>>>> {
    match dir.to_ascii_lowercase().as_ref() {
        "align" => {
            let align = parse_next_token!(int: iter => u32)?;
            Ok(Directive::Align(align))
        }
        "byte" => {
            let byte = parse_next_token!(int: iter => u8)?;
            Ok(Directive::Byte(byte))
        }
        "equ" => {
            let symbol = parse_next_token!(expr: iter)?;
            let value = parse_next_token!(int: iter => u32)?;
            Ok(Directive::Equ(symbol, value))
        }
        "halfword" => {
            let halfword = parse_next_token!(int: iter => u16)?;
            Ok(Directive::Halfword(halfword))
        }
        "include" => {
            let include = parse_next_token!(str: iter)?;
            Ok(Directive::Include(include))
        }
        "word" => {
            let word = parse_next_token!(int: iter => u32)?;
            Ok(Directive::Word(word))
        }
        "nsection" => {
            let name = parse_next_token!(expr: iter)?;
            let addr = parse_next_token!(optint: iter => u32);
            Ok(Directive::Section(SecurityMode::NoSecure, name, addr))
        }
        "lsection" => {
            let name = parse_next_token!(expr: iter)?;
            let addr = parse_next_token!(optint: iter => u32);
            Ok(Directive::Section(SecurityMode::LightSecure, name, addr))
        }
        "hsection" => {
            let name = parse_next_token!(expr: iter)?;
            let addr = parse_next_token!(optint: iter => u32);
            Ok(Directive::Section(SecurityMode::HeavySecure, name, addr))
        }
        "skip" => {
            let skip = parse_next_token!(int: iter => u32)?;
            let filler = parse_next_token!(optint: iter => u8);
            Ok(Directive::Skip(skip, filler))
        }
        "str" => {
            let string = parse_next_token!(str: iter)?;
            Ok(Directive::Str(string))
        }
        _ => Err(None),
    }
}

#[derive(Debug)]
pub struct Context<'a> {
    pub context_name: OsString,

    directives: Vec<Directive<'a>>,
    symbols: BTreeMap<String, Symbol>,
    sections: Vec<Section<'a>>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Context {
            context_name: OsString::from("<<main>>"),
            directives: Vec::new(),
            symbols: BTreeMap::new(),
            sections: vec![Section::default()],
        }
    }

    // Sets the name of the current assembler context.
    //
    // If invoked, this ideally should be the name of the source
    // file being processed.
    pub fn set_context_name(&mut self, name: OsString) {
        self.context_name = name;
    }

    // Adds a new directive to the end of the internal cache.
    pub fn add_directive(&mut self, dir: Directive<'a>) {
        self.directives.push(dir);
    }

    // Gets the first directive from the internal cache and
    // removes it.
    //
    // Ideally speaking, this yields directives in the order they
    // were inserted.
    pub fn get_directive(&mut self) -> Directive<'a> {
        self.directives.remove(0)
    }

    // Adds a new section to the end of the internal cache given its
    // metadata.
    pub fn add_section(&mut self, name: &'a str, mode: SecurityMode, addr: Option<u32>) {
        let section = Section::new(name, mode, addr.unwrap_or(0));
        self.sections.push(section);
    }

    // Gets the first section from the internal cache and
    // removes it.
    //
    // Ideally speaking, this yields sections in the order they
    // were inserted.
    pub fn get_section(&mut self) -> Option<Section<'a>> {
        if self.sections.is_empty() {
            None
        } else {
            Some(self.sections.remove(0))
        }
    }

    // Gets an immutable reference to the currently processed section.
    pub fn current_section(&self) -> &Section<'a> {
        // SAFETY: Since Context is always initialized with at least
        // a single default section, this will never cause a panic.
        self.sections.last().unwrap()
    }

    // Gets a mutable reference to the currently processed section.
    pub fn current_section_mut(&mut self) -> &mut Section<'a> {
        // SAFETY: Since Context is always initialized with at least
        // a single default section, this will never cause a panic.
        self.sections.last_mut().unwrap()
    }

    // Adds a new label to the end of the internal symbol cache given its name.
    pub fn add_label(&mut self, name: &'a str) {
        let symbol = Symbol::label(self.current_section());
        self.symbols.insert(
            [self.current_section().mode.get_prefix(), name].concat(),
            symbol,
        );
    }

    // Adds a new declaration to the end of the internal symbol cache given its
    // name and value.
    pub fn add_declaration(&mut self, name: &'a str, value: u32) {
        let symbol = Symbol::declaration(self.current_section(), value);
        self.symbols.insert(name.to_owned(), symbol);
    }
}

// A section in the microcode program text.
//
// Sections are represented as sequences of memory containing either code
// or data and are uniquely identified by a name. Further, sections can
// be relocated to desired virtual addresses and denote in which security
// mode they should be executed at runtime.
#[derive(Debug, PartialEq, Eq)]
pub struct Section<'a> {
    // The name of the section.
    pub name: &'a str,
    // The security mode that is assigned to this section.
    pub mode: SecurityMode,
    // The virtual base address in memory at which the section starts.
    pub base: u32,
    // Count of instructions within the section.
    pub counter: u32,
    // The current position within the `code` buffer at which to peek the
    // next token.
    pub peek_index: usize,
    // The program source code within the section.
    code: Vec<ParseSpan<Token<'a>>>,
}

impl<'a> Section<'a> {
    // Creates a new section from its metadata.
    pub fn new(name: &'a str, mode: SecurityMode, base: u32) -> Self {
        Section {
            name,
            mode,
            base,
            counter: 0,
            peek_index: 0,
            code: Vec::new(),
        }
    }

    // Adds a new token to the code within this section.
    pub fn add_code_token(&mut self, token: ParseSpan<Token<'a>>) {
        self.code.push(token);
    }

    // Gets the next code token of the section, without removing it from
    // the internal cache.
    pub fn peek_code_token(&mut self) -> Option<&ParseSpan<Token<'a>>> {
        let temp = self.peek_index;
        self.peek_index += 1;

        self.code.get(temp)
    }

    // Gets the first token from the internal code cache of this
    // section.
    //
    // Ideally speaking, this yields tokens in the order they were
    // inserted.
    pub fn get_code_token(&mut self) -> Option<ParseSpan<Token<'a>>> {
        if self.code.is_empty() {
            None
        } else {
            self.peek_index = 0;
            Some(self.code.remove(0))
        }
    }
}

impl<'a> Default for Section<'a> {
    fn default() -> Self {
        Section {
            name: "__default__",
            mode: SecurityMode::NoSecure,
            base: 0,
            counter: 0,
            peek_index: 0,
            code: Vec::new(),
        }
    }
}

// Representation of the different Falcon security modes.
//
// These are used to determine the context of symbols and sections to internally
// allow multiple occurrences across different security modes. This is necessary
// since jumping from secure mode to a symbol in insecure mode would cause the
// processor to transition back to insecure mode.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SecurityMode {
    // The Falcon No Secure mode. This is the only mode unsigned microcode can
    // execute code in directly.
    NoSecure,
    // The Falcon Light Secure mode. Must be entered from Heavy Secure context
    // and grants some debugging possibilities for secure code.
    LightSecure,
    // The Falcon Heavy Secure mode. Microcode in this mode is granted the highest
    // possible set of privileges while, at the same time, all debugging features
    // are disabled.
    HeavySecure,
}

impl SecurityMode {
    fn get_prefix(&self) -> &'static str {
        match self {
            SecurityMode::NoSecure => "ns_",
            SecurityMode::LightSecure => "ls_",
            SecurityMode::HeavySecure => "hs_",
        }
    }
}

// A symbol declared in assembly.
//
// Symbols may either be declarations using the `EQU` directive which
// have a value assigned to them, or labels mostly used for branches.
#[derive(Debug, PartialEq, Eq)]
pub struct Symbol {
    // The position at which the label appears.
    pub position: u32,
    // An optional value that is assigned to the symbol.
    pub value: Option<u32>,
}

impl Symbol {
    // Constructs a new symbol from a label token.
    fn label(section: &Section) -> Self {
        Symbol {
            position: section.base + section.counter,
            value: None,
        }
    }

    // Constructs a new symbol from a declaration using `EQU`.
    fn declaration(section: &Section, value: u32) -> Self {
        Symbol {
            position: section.base + section.counter,
            value: Some(value),
        }
    }
}

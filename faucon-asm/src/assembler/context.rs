use std::collections::HashMap;
use std::ffi::OsString;
use std::iter::Peekable;

use num_traits::{cast, NumCast};

use crate::arguments::Argument;
use crate::assembler::lexer::Token;
use crate::assembler::span::ParseSpan;
use crate::opcode::OperandSize;

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

// A relocation for a forward branch yet to be resolved.
//
// Due to variable-length instructions, we may not know the memory address
// of a label at the time it occurs. That's when we use relocations to keep
// track of the symbol and leave zeroes as a filler for the jump offset.
// After the first assembler pass when all symbols should be known, we can
// patch these relocations and insert correct jump offsets at the desired
// positions of the output code buffer.
#[derive(Debug)]
pub struct Relocation<'a> {
    // The address this relocation points to. This may be patched at a later
    // time before the value is accurate.
    pub address: u32,
    // The position within the code buffer where the relocation needs to be
    // inserted. This may be patched at a later time.
    pub position: u32,
    is_patched: bool,
    // The name of the symbol that is associated with this relocation.
    pub symbol: &'a str,
    // The argument that will be used to patch the emitted code with the real
    // address of the relocation later on.
    pub argument: Argument,
}

impl<'a> Relocation<'a> {
    // Initializes a relocation for a new symbol.
    pub fn new(address: u32, symbol: &'a str, arg: &Argument, size: &OperandSize) -> Self {
        let argument = if let Argument::SizeConverter(c) = arg {
            c(size.value())
        } else {
            arg.clone()
        };

        Relocation {
            address,
            position: address,
            is_patched: false,
            symbol,
            argument,
        }
    }

    // Patches the relocation with correct address and base offsets.
    pub fn patch(&mut self, address_base: u32, position_base: u32) {
        if !self.is_patched {
            self.address += address_base;
            self.position += position_base;
            self.is_patched = true;
        }
    }
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
fn try_extract_symbol_ident<'a>(
    span: ParseSpan<Token<'a>>,
) -> Result<&'a str, ParseSpan<Token<'a>>> {
    match span.token() {
        Token::Symbol(s) => Ok(s),
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
            .and_then(|s| try_extract_symbol_ident(s).map_err(Some))
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
    symbols: HashMap<String, Symbol>,
    sections: Vec<Section<'a>>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        let mut sections = Vec::with_capacity(5);
        sections.push(Section::default());

        Context {
            context_name: OsString::from("<<main>>"),
            directives: Vec::with_capacity(15),
            symbols: HashMap::with_capacity(30),
            sections,
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

    // Gets a mutable reference to the currently processed section.
    pub fn current_section(&mut self) -> &mut Section<'a> {
        // SAFETY: Since Context is always initialized with at least
        // a single default section, this will never cause a panic.
        self.sections.last_mut().unwrap()
    }

    // Adds a new label to the end of the internal symbol cache given its name.
    //
    // If this does not return None, a symbol of the same name has been declared
    // previously and the assembler should abort due to multiple definitions of
    // the same name.
    pub fn add_label(&mut self, name: &'a str) -> Result<(), ()> {
        self.symbols
            .insert(name.to_owned(), Symbol::default())
            .map_or_else(|| Ok(()), |_| Err(()))
    }

    // Updates the value of a label symbol.
    //
    // Returns the address of the label on success, or an empty error if the label
    // could not be found in the internal symbol cache.
    pub fn set_label_address(&mut self, name: &'a str, new_addr: u32) -> Result<u32, ()> {
        self.symbols
            .get_mut(name)
            .and_then(|e| {
                e.0 = new_addr;
                e.1 = true;
                Some(new_addr)
            })
            .ok_or_else(|| ())
    }

    // Adds a new declaration to the end of the internal symbol cache given its
    // name and value.
    //
    // If this does not return None, a symbol of the same name has been declared
    // previously and the assembler should abort due to multiple definitions of
    // the same name.
    pub fn add_declaration(&mut self, name: &'a str, value: u32) -> Result<(), ()> {
        self.symbols
            .insert(name.to_owned(), Symbol(value, true))
            .map_or_else(|| Ok(()), |_| Err(()))
    }

    // Attempts to look up a symbol from the internal cache by name and returns its
    // value, if present.
    pub fn find_symbol(&self, name: &'a str) -> Option<u32> {
        self.symbols
            .get(name)
            .and_then(|s| if s.1 { Some(s.0) } else { None })
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
    // The local instruction counter within the section.
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
            code: Vec::with_capacity(100),
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

// An assembly symbol that is either a label or a declaration through the `EQU`
// directive.
//
// In either case, a symbol has a value assigned to it and tracks if said value
// has been resolved already via the second field.
#[derive(Clone, Copy, Debug)]
pub struct Symbol(pub u32, pub bool);

impl Default for Symbol {
    fn default() -> Self {
        Symbol(0, false)
    }
}

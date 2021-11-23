use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token, Attribute, Error, Ident, LitInt, Result, Token,
};

mod kw {
    syn::custom_keyword!(opcode);
    syn::custom_keyword!(subopcode);
    syn::custom_keyword!(operands);
}

#[derive(Clone)]
pub struct Attrs<'a> {
    pub insn: Vec<InsnAttr<'a>>,
}

impl<'a> Attrs<'a> {
    pub fn get(input: &'a [Attribute]) -> Result<Attrs<'a>> {
        let mut attrs = Attrs { insn: Vec::new() };

        for attr in input {
            if attr.path.is_ident("insn") {
                parse_insn_attr(&mut attrs, attr)?;
            }
        }

        Ok(attrs)
    }
}

/// #[insn(...)]
///
/// Only on enum variants.
#[derive(Clone)]
pub struct InsnAttr<'a> {
    pub original: &'a Attribute,
    /// #[insn(opcode = 0xDF)]
    pub opcode: Option<u8>,
    /// #[insn(subopcode = 0xAB)]
    pub subopcode: Option<u8>,
    /// #[insn(operands(A, B, C))]
    pub operands: Option<Vec<Ident>>,
}

fn parse_insn_attr<'a>(attrs: &mut Attrs<'a>, attr: &'a Attribute) -> Result<()> {
    attrs.insn.push(InsnAttr {
        original: attr,
        opcode: None,
        subopcode: None,
        operands: None,
    });
    let insn = attrs.insn.last_mut().unwrap();

    attr.parse_args_with(|input: ParseStream| {
        let mut first = true;
        while !input.is_empty() {
            if !first {
                input.parse::<Token![,]>()?;
            }

            let look = input.lookahead1();
            if look.peek(kw::opcode) {
                if insn.opcode.is_some() {
                    return Err(Error::new_spanned(
                        attr,
                        "duplicate #[insn(opcode)] attribute found",
                    ));
                }

                let AttrWrapper::<kw::opcode, LitInt> { value, .. } = input.parse()?;
                insn.opcode = Some(value.base10_parse()?);
            } else if look.peek(kw::subopcode) {
                if insn.subopcode.is_some() {
                    return Err(Error::new_spanned(
                        attr,
                        "duplicate #[insn(subopcode)] attribute found",
                    ));
                }

                let AttrWrapper::<kw::subopcode, LitInt> { value, .. } = input.parse()?;
                insn.subopcode = Some(value.base10_parse()?);
            } else if look.peek(kw::operands) {
                if insn.operands.is_some() {
                    return Err(Error::new_spanned(
                        attr,
                        "duplicate #[insn(operands)] attribute found",
                    ));
                }

                input.parse::<kw::operands>()?;
                let content;
                parenthesized!(content in input);
                let value: Punctuated<Ident, Token![,]> = content.parse_terminated(Ident::parse)?;
                insn.operands = Some(value.into_iter().collect());
            } else {
                return Err(look.error());
            }

            first = false;
        }
        Ok(())
    })
}

#[allow(unused)]
struct AttrWrapper<K: Parse, V: Parse> {
    pub ident: K,
    pub value: V,
}

impl<K: Parse, V: Parse> Parse for AttrWrapper<K, V> {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse()?;
        let value = if input.peek(token::Paren) {
            // #[ident(value)]
            let value;
            parenthesized!(value in input);
            value.parse()?
        } else {
            // #[ident = value]
            input.parse::<Token![=]>()?;
            input.parse()?
        };

        Ok(AttrWrapper { ident, value })
    }
}

use syn::{Data, DataEnum, DeriveInput, Error, Generics, Ident, Result};

use crate::attrs::Attrs;

pub fn get(node: &DeriveInput) -> Result<Enum<'_>> {
    match &node.data {
        Data::Enum(data) => Enum::from_syn(node, data),
        Data::Struct(_) | Data::Union(_) => Err(Error::new_spanned(
            node,
            "#[derive(Instruction)] only allowed on enums",
        )),
    }
}

pub struct Enum<'a> {
    pub original: &'a DeriveInput,
    pub ident: Ident,
    pub generics: &'a Generics,
    pub variants: Vec<Variant<'a>>,
}

impl<'a> Enum<'a> {
    fn from_syn(node: &'a DeriveInput, data: &'a DataEnum) -> Result<Self> {
        let variants = data
            .variants
            .iter()
            .map(Variant::from_syn)
            .collect::<Result<_>>()?;

        Ok(Enum {
            original: node,
            ident: node.ident.clone(),
            generics: &node.generics,
            variants,
        })
    }

    pub fn validate(&self) -> Result<()> {
        for variant in &self.variants {
            variant.validate()?;
        }
        Ok(())
    }
}

pub struct Variant<'a> {
    pub original: &'a syn::Variant,
    pub attrs: Attrs<'a>,
    pub ident: Ident,
}

impl<'a> Variant<'a> {
    fn from_syn(node: &'a syn::Variant) -> Result<Self> {
        let attrs = Attrs::get(&node.attrs)?;
        Ok(Variant {
            original: node,
            attrs,
            ident: node.ident.clone(),
        })
    }

    fn validate(&self) -> Result<()> {
        if self.attrs.insn.is_empty() {
            return Err(Error::new_spanned(
                self.original,
                "variant needs at least one #[insn] attribute on it",
            ));
        }
        Ok(())
    }
}

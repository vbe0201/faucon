use proc_macro2::{Span, TokenStream};
use syn::{Attribute, Error, Meta, Result};

use crate::r#impl::ATTR;

/// Flattens all the meta properties within a given attribute into a vector.
///
/// NOTE: Literals will not be considered and result in an error.
pub fn flatten_attribute_meta(attr: &Attribute) -> Result<Vec<Meta>> {
    if let Meta::List(ref list) = attr.parse_meta()? {
        let mut properties = Vec::new();

        for nested_meta in list.nested.iter() {
            if let syn::NestedMeta::Meta(meta) = nested_meta {
                properties.push(meta.clone());
            } else {
                return Err(Error::new(
                    attr.path.segments[0].ident.span(),
                    format!("\"{}\" does not expect any literal properties", ATTR),
                ));
            }
        }

        Ok(properties)
    } else {
        Err(Error::new(
            attr.path.segments[0].ident.span(),
            format!("\"{}\" should be used as a list-style attribute", ATTR),
        ))
    }
}

/// Parses a `name=value` property inside a proc-macro attribute into an integer of
/// `value`.
pub fn parse_int_meta(name: &str, meta: &Meta) -> Result<u8> {
    if let Meta::NameValue(ref value) = meta {
        assert_matching_name(name, &value.path)?;
        if let syn::Lit::Int(ref int) = value.lit {
            Ok(int.base10_parse()?)
        } else {
            Err(Error::new(
                Span::call_site(),
                format!("The \"{}\" property must be a number", name),
            ))
        }
    } else {
        Err(Error::new(
            Span::call_site(),
            format!(
                "The \"{0}\" property must be a name-value pair: {0} = ...",
                name
            ),
        ))
    }
}

/// Parses a `name(a, b, c)` property inside a proc-macro attribute into a vector
/// containing `a, b, c`.
pub fn parse_list_meta(name: &str, meta: &Meta) -> Result<Vec<TokenStream>> {
    if let Meta::List(ref list) = meta {
        assert_matching_name(name, &list.path)?;
        let tokens = list
            .nested
            .iter()
            .map(|e| quote! { Some(#e) })
            .collect::<Vec<TokenStream>>();

        Ok(tokens)
    } else {
        Err(Error::new(
            Span::call_site(),
            format!(
                "The \"{0}\" property must be a list containing items: {0}(...)",
                name
            ),
        ))
    }
}

fn assert_matching_name(name: &str, path: &syn::Path) -> Result<()> {
    if path.is_ident(&syn::Ident::new(name, Span::call_site())) {
        Ok(())
    } else {
        Err(Error::new(
            Span::call_site(),
            format!(
                "Expected a property named \"{}\" inside the proc-macro attribute",
                name
            ),
        ))
    }
}

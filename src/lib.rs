//! # derive_enum_methods
//!
//! This is a fork of [derive_is_enum_variant](https://github.com/fitzgen/derive_is_enum_variant) by @fitzgen that can additionally derive `as_*` and `unsafe as_*_unchecked` methods.
//! ## `derive_enum_methods`
//!
//!
//! ### Usage
//!
//! Add `derive_enum_methods` to your crate's `Cargo.toml`:
//!
//! ```toml
//! [dependencies]
//! derive_enum_methods = { git = "https://github.com/ves-lang/derive_enum_methods" }
//! ```
//!
//! And then add `#[derive(is_enum_variant, as_enum_variant, enum_variant_unchecked)]` to your `enum` definitions:
//!
//! ```rust,ignore
//! #[macro_use]
//! extern crate derive_enum_methods;
//!
//! struct Doggo;
//! struct Kitten;
//!
//! // This derive ...
//! #[derive(is_enum_variant, as_enum_variant, enum_variant_unchecked(mut))]
//! pub enum Pet {
//!     Doggo(Doggo),
//!     Kitten(Kitten),
//! }
//!
//! // will generate the code below:
//! impl Pet {
//!     fn is_doggo(&self) -> bool {
//!         matches!(self, Self::Doggo { .. })
//!     }
//!     fn is_kitten(&self) -> bool {
//!         matches!(self, Self::Kitten { .. })
//!     }
//! }
//! impl Pet {
//!     fn as_doggo(&self) -> Option<&Doggo> {
//!         if Self::Doggo(__self_0) = self {
//!             Some(__self_0)
//!         } else {
//!             None
//!         }
//!     }
//!     fn as_doggo_mut(&mut self) -> Option<&mut Doggo> {
//!         if Self::Doggo(__self_0) = self {
//!             Some(__self_0)
//!         } else {
//!             None
//!         }
//!     }
//!     fn as_kitten(&self) -> Option<&Kitten> {
//!         if Self::Kitten(__self_0) = self {
//!             Some(__self_0)
//!         } else {
//!             None
//!         }
//!     }
//!     fn as_kitten_mut(&mut self) -> Option<&mut Kitten> {
//!         if Self::Kitten(__self_0) = self {
//!             Some(__self_0)
//!         } else {
//!             None
//!         }
//!     }
//! }
//! impl Pet {
//!     unsafe fn as_doggo_unchecked(&self) -> &Doggo {
//!         match self {
//!             Self::Doggo(__self_0) => __self_0,
//!             _ => {
//!                 if cfg!(debug_assertions) {
//!                     unreachable!()
//!                 } else {
//!                     core::hint::unreachable_unchecked()
//!                 }
//!             }
//!         }
//!     }
//!     unsafe fn as_doggo_unchecked_mut(&mut self) -> &mut Doggo {
//!         match self {
//!             Self::Doggo(__self_0) => __self_0,
//!             _ => {
//!                 if cfg!(debug_assertions) {
//!                     unreachable!()
//!                 } else {
//!                     core::hint::unreachable_unchecked()
//!                 }
//!             }
//!         }
//!     }
//!     unsafe fn as_kitten_unchecked(&self) -> &Kitten {
//!         match self {
//!             Self::Kitten(__self_0) => __self_0,
//!             _ => {
//!                 if cfg!(debug_assertions) {
//!                     unreachable!()
//!                 } else {
//!                     core::hint::unreachable_unchecked()
//!                 }
//!             }
//!         }
//!     }
//!     unsafe fn as_kitten_unchecked(&mut self) -> &mut Kitten {
//!         match self {
//!             Self::Kitten(__self_0) => __self_0,
//!             _ => {
//!                 if cfg!(debug_assertions) {
//!                     unreachable!()
//!                 } else {
//!                     core::hint::unreachable_unchecked()
//!                 }
//!             }
//!         }
//!     }
//! }
//! ```
//!
//! #### Customizing Predicate Names
//!
//! By default, the predicates are named `is_snake_case_of_variant_name`. You can
//! use any name you want instead with `#[is_enum_variant(name = "..")]` (same for the other two attributes):
//!
//! ```rust
//! use derive_enum_methods::is_enum_variant;
//!
//! #[derive(is_enum_variant)]
//! pub enum Pet {
//!     #[is_enum_variant(name = "isDoggo")]
//!     Doggo,
//!     Kitten,
//! }
//!
//! let pet = Pet::Doggo;
//! assert!(pet.isDoggo());
//! ```
//!
//! #### Skipping Predicates for Certain Variants
//!
//! If you don't want to generate a predicate for a certain variant, you can use
//! `#[is_enum_variant(skip)]` (same for the other two attributes):
//!
//! ```rust,ignore
//! #[derive(is_enum_variant)]
//! pub enum Errors {
//!     Io(::std::io::Error),
//!
//!     #[doc(hidden)]
//!     #[is_enum_variant(skip)]
//!     __NonExhaustive,
//! }
//!
//! ```
//!
//! ### License
//!
//! Licensed under either of
//!
//!   * Apache License, Version 2.0 ([`LICENSE-APACHE`](./LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
//!   * MIT license ([`LICENSE-MIT`](./LICENSE-MIT) or http://opensource.org/licenses/MIT)
//!
//! at your option.
//!
//! ### Contribution
//!
//! See [CONTRIBUTING.md](./CONTRIBUTING.md) for hacking!
//!
//! Unless you explicitly state otherwise, any contribution intentionally submitted
//! for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
//! dual licensed as above, without any additional terms or conditions.
//!
extern crate heck;
extern crate proc_macro;
#[macro_use]
extern crate quote;
extern crate syn;

use heck::SnakeCase;
use proc_macro::TokenStream;
use syn::spanned::Spanned;

macro_rules! syn_try {
    ($e:expr) => {
        match $e {
            Ok(ok) => ok,
            Err(e) => return e.to_compile_error().into(),
        }
    };
    ($e:expr, $msg:expr) => {
        match $e {
            Ok(ok) => ok,
            Err(e) => {
                return e
                    .combine(Error::new(e.span(), $msg))
                    .to_compile_error()
                    .into()
            }
        }
    };
}

#[proc_macro_derive(is_enum_variant, attributes(is_enum_variant, is_enum_variant_mut))]
pub fn derive_is_enum_variant(tokens: TokenStream) -> TokenStream {
    let ast = syn_try!(syn::parse::<syn::DeriveInput>(tokens));
    expand_derive_impl(&ast, EnumDeriveKind::Is)
}

#[proc_macro_derive(as_enum_variant, attributes(as_enum_variant))]
pub fn derive_as_enum_variant(tokens: TokenStream) -> TokenStream {
    let ast = syn_try!(syn::parse::<syn::DeriveInput>(tokens));
    expand_derive_impl(&ast, EnumDeriveKind::As)
}

#[proc_macro_derive(enum_variant_unchecked, attributes(enum_variant_unchecked))]
pub fn derive_enum_variant_unchecked(tokens: TokenStream) -> TokenStream {
    let ast = syn_try!(syn::parse::<syn::DeriveInput>(tokens));
    expand_derive_impl(&ast, EnumDeriveKind::Unchecked)
}

enum PredicateConfig {
    None,
    Skip,
    Name(String),
}

fn path_to_string(path: &syn::Path) -> String {
    path.segments
        .iter()
        .map(|s| s.ident.to_string())
        .collect::<Vec<_>>()
        .join("")
}

impl PredicateConfig {
    fn join(self, attr: &str, meta: &syn::Meta) -> Self {
        match meta {
            syn::Meta::Path(ident) if path_to_string(ident) == "skip" => match self {
                PredicateConfig::None | PredicateConfig::Skip => PredicateConfig::Skip,
                PredicateConfig::Name(_) => panic!(
                    "Cannot use both `#[{0}(skip)]` and \
                     `#[{0}(name = \"..\")]`",
                    attr
                ),
            },
            syn::Meta::NameValue(syn::MetaNameValue {
                path,
                lit: syn::Lit::Str(ref s),
                ..
            }) if path_to_string(&path) == "name" => {
                if !s
                    .value()
                    .chars()
                    .all(|c| matches!(c, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'))
                {
                    panic!(
                        "#[{}(name = \"..\")] must be provided \
                         a valid identifier",
                        attr
                    )
                }
                match self {
                    PredicateConfig::None => PredicateConfig::Name(s.value()),
                    PredicateConfig::Skip => panic!(
                        "Cannot both `#[{0}(skip)]` and \
                         `#[{0}(name = \"..\")]`",
                        attr
                    ),
                    PredicateConfig::Name(_) => panic!(
                        "Cannot provide more than one \
                         `#[{}(name = \"..\")]`",
                        attr
                    ),
                }
            }
            ref otherwise => panic!(
                "Unknown item inside `#[{}(..)]`: {:?}",
                attr,
                quote! { #otherwise }.to_string()
            ),
        }
    }
}

impl<'a> PredicateConfig {
    fn from(attrs: &[syn::Attribute], attr: &str) -> Result<Self, syn::Error> {
        let out_attr = attrs.iter().find(|a| path_to_string(&a.path) == attr);
        if let Some(out) = out_attr {
            out.parse_meta().map(|meta| match meta {
                syn::Meta::List(list) => list
                    .nested
                    .iter()
                    .map(|m| match *m {
                        syn::NestedMeta::Meta(ref m) => m,
                        syn::NestedMeta::Lit(_) => panic!("Invalid #[{}] item", attr),
                    })
                    .fold(PredicateConfig::None, |acc, meta| acc.join(attr, meta)),
                _ => panic!(
                    "#[{0}] must be used with name/value pairs, like \
                     #[{0}(name = \"..\")]",
                    attr
                ),
            })
        } else {
            Ok(PredicateConfig::None)
        }
    }
}
enum EnumDeriveKind {
    Is,
    As,
    Unchecked,
}

impl EnumDeriveKind {
    fn name(&self) -> &'static str {
        match self {
            EnumDeriveKind::Is => "is_enum_variant",
            EnumDeriveKind::As => "as_enum_variant",
            EnumDeriveKind::Unchecked => "unchecked_enum_variant",
        }
    }

    fn name_for(&self, variant: String) -> String {
        match self {
            EnumDeriveKind::Is => {
                format!("is_{}", variant)
            }
            EnumDeriveKind::As => {
                format!("as_{}", variant)
            }
            EnumDeriveKind::Unchecked => {
                format!("as_{}_unchecked", variant)
            }
        }
    }

    fn doc_for(&self, variant_name: &str) -> String {
        match self {
            EnumDeriveKind::Is => {
                format!("Returns `true` if the value is [`{}`].", variant_name)
            }
            EnumDeriveKind::As => {
                format!("Returns `Some(value)` the value is [`{}`].", variant_name)
            }
            EnumDeriveKind::Unchecked => {
                format!("Returns the payload of the [`{}`] variant without checking whether this is correct in release builds. \
                In debug builds this method will panic if the variant is incorrect.\n\n#SAFETY\n \
                The caller *must* ensure that `self` is the right variant. Failure to do so _will_ cause data corruption and hard to debug problems. \
                See the [unreachable_unchecked](https://doc.rust-lang.org/core/hint/fn.unreachable_unchecked.html) for more info.", variant_name)
            }
        }
    }
}

fn expand_derive_impl(ast: &syn::DeriveInput, kind: EnumDeriveKind) -> TokenStream {
    let data = match ast.data {
        syn::Data::Enum(ref variants) => variants,
        _ => panic!("#[derive({})] can only be used with enums", kind.name()),
    };

    let name = &ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    let mut methods = Vec::with_capacity(data.variants.len());
    for variant in &data.variants {
        let syn::Variant {
            ref ident,
            ref fields,
            ref attrs,
            ..
        } = variant;

        let cfg = syn_try!(PredicateConfig::from(attrs, kind.name()));
        if let PredicateConfig::Skip = cfg {
            continue;
        }

        let variant_name = ident.to_string();
        let doc = kind.doc_for(&variant_name);

        let predicate_name = if let PredicateConfig::Name(name) = cfg {
            name
        } else {
            kind.name_for(variant_name.to_snake_case())
        };
        let predicate_name_mut = syn::Ident::new(&format!("{}_mut", predicate_name), ident.span());
        let predicate_name = syn::Ident::new(&predicate_name, ident.span());

        let (field_bindings, return_type, return_type_mut) = if matches!(kind, EnumDeriveKind::Is) {
            (
                vec![match fields {
                    syn::Fields::Named(..) => quote! { { .. } },
                    syn::Fields::Unnamed(..) => quote! { (..) },
                    syn::Fields::Unit => quote! {},
                }],
                quote! {},
                quote! {},
            )
        } else {
            let field_types = fields.iter().map(|f| &f.ty).collect::<Vec<_>>();
            if field_types.is_empty() {
                // This is a no-payload enum, so we can't generate a getter
                (Vec::new(), quote! {()}, quote! {()})
            } else if field_types.len() == 1 {
                let bindings = generate_bindings(fields);
                let ty = field_types.first().unwrap();
                (bindings, quote! { &#ty }, quote! { &mut #ty })
            } else {
                let bindings = generate_bindings(fields);
                (
                    bindings,
                    quote! { ( #( &#field_types ),* )},
                    quote! { ( #( &mut #field_types ),* )},
                )
            }
        };

        if !matches!(kind, EnumDeriveKind::Is) && field_bindings.is_empty() {
            continue;
        }

        let field_names = field_bindings
            .iter()
            .enumerate()
            .map(|(i, s)| syn::Ident::new(&format!("__self_{}", i), s.span()))
            .collect::<Vec<_>>();

        let field_list = if field_names.len() == 1 {
            let field = field_names.first().unwrap();
            quote! { &#field }
        } else {
            quote! {
               ( #( &#field_names ),* )
            }
        };
        let field_list_mut = if field_names.len() == 1 {
            let field = field_names.first().unwrap();
            quote! { &mut *#field }
        } else {
            quote! {
               ( #( &mut *#field_names ),* )
            }
        };

        methods.push(match kind {
            EnumDeriveKind::Is => {
                quote! {
                    #[doc = #doc]
                    #[inline]
                    #[allow(unreachable_patterns)]
                    #[allow(dead_code)]
                    pub fn #predicate_name(&self) -> bool {
                        matches!(self, Self :: #ident #( #field_bindings )*)
                    }
                }
            }
            EnumDeriveKind::As => {
                quote! {
                    #[doc = #doc]
                    #[inline]
                    #[allow(unreachable_patterns)]
                    #[allow(dead_code)]
                    pub fn #predicate_name(&self) -> Option<#return_type> {
                        if let Self :: #ident { #( #field_bindings ),* } = self {
                            Some( #field_list )
                        } else {
                            None
                        }
                    }

                    #[doc = #doc]
                    #[inline]
                    #[allow(unreachable_patterns)]
                    #[allow(dead_code)]
                    pub fn #predicate_name_mut(&mut self) -> Option<#return_type_mut> {
                        if let Self :: #ident { #( #field_bindings ),* } = self {
                            Some( #field_list_mut )
                        } else {
                            None
                        }
                    }
                }
            }
            EnumDeriveKind::Unchecked => {
                quote! {
                    #[doc = #doc]
                    #[inline]
                    #[allow(unreachable_patterns)]
                    #[allow(dead_code)]
                    pub unsafe fn #predicate_name(&self) -> #return_type {
                        match self {
                            Self :: #ident { #( #field_bindings ),* } => {
                                #field_list
                            }
                            _ => {
                                if cfg!(debug_assertions) {
                                    unreachable!()
                                } else {
                                    ::std::hint::unreachable_unchecked()
                                }
                            }
                        }
                    }

                    #[doc = #doc]
                    #[inline]
                    #[allow(unreachable_patterns)]
                    #[allow(dead_code)]
                    pub unsafe fn #predicate_name_mut(&mut self) -> #return_type_mut {
                        match self {
                            Self :: #ident { #( #field_bindings ),* } => {
                                #field_list_mut
                            }
                            _ => {
                                if cfg!(debug_assertions) {
                                    unreachable!()
                                } else {
                                    ::std::hint::unreachable_unchecked()
                                }
                            }
                        }
                    }
                }
            }
        });
    }

    (quote! {
        impl #impl_generics #name #ty_generics #where_clause {
            #(
                #methods
            )*
        }
    })
    .into()
}

fn generate_bindings(fields: &syn::Fields) -> Vec<quote::__private::TokenStream> {
    match fields {
        syn::Fields::Named(syn::FieldsNamed { named: fields, .. })
        | syn::Fields::Unnamed(syn::FieldsUnnamed {
            unnamed: fields, ..
        }) => fields
            .iter()
            .enumerate()
            .map(|(i, f)| {
                let binding = syn::Ident::new(&format!("__self_{}", i), f.span());
                if let Some(name) = &f.ident {
                    quote! { #name: #binding }
                } else {
                    let index = syn::Index::from(i);
                    quote! { #index: #binding }
                }
            })
            .collect(),
        syn::Fields::Unit => Vec::new(),
    }
}

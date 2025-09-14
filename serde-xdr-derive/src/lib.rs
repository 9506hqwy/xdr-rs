use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{Fields, ItemEnum, Type, parse};

#[proc_macro_derive(XdrIndexer)]
pub fn xdr_indexer(input: TokenStream) -> TokenStream {
    let ast: ItemEnum = parse(input).unwrap();

    let ident = &ast.ident;

    let mut ser_match_arms = vec![];
    let mut de_match_arms = vec![];
    for variant in ast.variants.iter() {
        let variant_ident = &variant.ident;
        match &variant.fields {
            Fields::Named(_) => {
                unimplemented!();
            }
            Fields::Unnamed(unnamed) => {
                let mut ser_args = vec![];
                let mut ser_stmts = vec![];
                let mut de_stmts = vec![];
                let mut de_rets = vec![];

                for (i, field) in unnamed.unnamed.iter().enumerate() {
                    let arg_ident = format_ident!("a{}", i);
                    let ret_ident = format_ident!("r{}", i);
                    let ty = &field.ty;
                    ser_args.push(quote! { #arg_ident });
                    ser_stmts.push(quote! {
                        ser.serialize_element(#arg_ident)?
                    });
                    de_stmts.push(quote! {
                        let #ret_ident = seq.next_element::<#ty>()?.unwrap()
                    });
                    de_rets.push(quote! { #ret_ident });
                }

                ser_match_arms.push(quote! {
                    #ident::#variant_ident(#(#ser_args),*) => {
                        #(#ser_stmts);*;
                    }
                });

                de_match_arms.push(quote! {
                    stringify!(#variant_ident) => {
                        #(#de_stmts);*;
                        Ok(#ident::#variant_ident(#(#de_rets),*))
                    }
                });
            }
            Fields::Unit => {
                ser_match_arms.push(quote! { #ident::#variant_ident => { } });
                de_match_arms
                    .push(quote! { stringify!(#variant_ident) => { Ok(#ident::#variant_ident) } });
            }
        }
    }

    let serde_ast = quote! {
        impl ::serde::Serialize for #ident {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: ::serde::ser::Serializer,
            {
                let index = self.index();

                let mut ser = serializer.serialize_tuple(index as usize)?;
                ser.serialize_element(&index)?;

                match self {
                    #(#ser_match_arms),*,
                    _ => unimplemented!(),
                }

                ser.end()
            }
        }

        impl<'de> ::serde::Deserialize<'de> for #ident {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: ::serde::de::Deserializer<'de>
            {
                struct XdrIndexerVisitor;

                impl<'de> ::serde::de::Visitor<'de> for XdrIndexerVisitor {
                    type Value = #ident;

                    fn expecting(&self, formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                        formatter.write_str(&format!("a {}", stringify!(#ident)))
                    }

                    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                    where
                        A: ::serde::de::SeqAccess<'de>,
                    {
                        let index = seq.next_element::<i32>()?.unwrap();
                        let variant = Self::Value::name_by_index(index)
                            .map_err(|_| ::serde::de::Error::custom(format!("Can not convert from {}", index)))?;
                        match variant {
                            #(#de_match_arms),*,
                            _ => unimplemented!(),
                        }
                    }
                }

                deserializer.deserialize_tuple(10, XdrIndexerVisitor)
            }
        }
    };

    serde_ast.into()
}

#[proc_macro_derive(XdrUnion)]
pub fn xdr_union(input: TokenStream) -> TokenStream {
    let ast: ItemEnum = parse(input).unwrap();

    let ident = &ast.ident;

    let mut ser_match_arms = vec![];
    let mut de_match_arms = vec![];
    let mut value_ty: Option<Type> = None;
    for variant in ast.variants.iter() {
        let variant_ident = &variant.ident;
        match &variant.fields {
            Fields::Named(_) => {
                unimplemented!();
            }
            Fields::Unnamed(unnamed) => {
                let mut ser_args = vec![];
                let mut ser_stmts = vec![];
                let mut de_stmts = vec![];
                let mut de_rets = vec![];

                let mut first = true;
                for (i, field) in unnamed.unnamed.iter().enumerate() {
                    let arg_ident = format_ident!("a{}", i);
                    let ret_ident = format_ident!("r{}", i);
                    let ty = &field.ty;

                    ser_args.push(quote! { #arg_ident });
                    ser_stmts.push(quote! {
                        ser.serialize_element(#arg_ident)?
                    });

                    if first {
                        de_rets.push(quote! { value });
                    } else {
                        de_stmts.push(quote! {
                            let #ret_ident = seq.next_element::<#ty>()?.unwrap()
                        });
                        de_rets.push(quote! { #ret_ident });
                    }

                    if value_ty.is_none() {
                        value_ty = Some(ty.clone());
                    }

                    first = false;
                }

                ser_match_arms.push(quote! {
                    #ident::#variant_ident(#(#ser_args),*) => {
                        #(#ser_stmts);*;
                    }
                });

                de_match_arms.push(quote! {
                    stringify!(#variant_ident) => {
                        #(#de_stmts);*;
                        Ok(#ident::#variant_ident(#(#de_rets),*))
                    }
                });
            }
            Fields::Unit => {
                ser_match_arms.push(quote! { #ident::#variant_ident => { } });
                de_match_arms
                    .push(quote! { stringify!(#variant_ident) => { Ok(#ident::#variant_ident) } });
            }
        }
    }

    let value_ty = value_ty.unwrap();
    let serde_ast = quote! {
        impl ::serde::Serialize for #ident {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: ::serde::ser::Serializer,
            {
                let mut ser = serializer.serialize_tuple(2usize)?;

                match self {
                    #(#ser_match_arms),*,
                    _ => unimplemented!(),
                }

                ser.end()
            }
        }

        impl<'de> ::serde::Deserialize<'de> for #ident {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: ::serde::de::Deserializer<'de>
            {
                struct XdrUnionVisitor;

                impl<'de> ::serde::de::Visitor<'de> for XdrUnionVisitor {
                    type Value = #ident;

                    fn expecting(&self, formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                        formatter.write_str(&format!("a {}", stringify!(#ident)))
                    }

                    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                    where
                        A: ::serde::de::SeqAccess<'de>,
                    {
                        let value = seq.next_element::<#value_ty>()?.unwrap();
                        let variant = Self::Value::name_by_value(&value)
                            .map_err(|_| ::serde::de::Error::custom(format!("Can not convert from {:?}", &value)))?;
                        match variant {
                            #(#de_match_arms),*,
                            _ => unimplemented!(),
                        }
                    }
                }

                deserializer.deserialize_tuple(10, XdrUnionVisitor)
            }
        }
    };

    serde_ast.into()
}

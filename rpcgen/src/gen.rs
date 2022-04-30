use crate::error::Error;
use crate::keyword;
use crate::parser::{
    Assign, Constant, Declaration, Definition, TypeDef, TypeSpecifier, UnionBody, Value,
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::collections::HashMap;
use std::convert::TryFrom;

#[derive(Clone, Default)]
pub struct Config {
    pub remove_typedef: bool,
}

struct Context {
    config: Config,
    constants: HashMap<String, u32>,
    typedefs: HashMap<String, TokenStream>,
}

impl Context {
    fn new(config: &Config) -> Self {
        Context {
            config: config.clone(),
            constants: HashMap::new(),
            typedefs: HashMap::new(),
        }
    }
}

pub fn gen(definitions: Vec<Definition>, config: &Config) -> Result<String, Error> {
    let mut cxt = Context::new(config);

    let constants = constants(&definitions, &mut cxt)?;
    let types = types(&definitions, &mut cxt)?;

    let binding = quote! {
        use serde::{Deserialize, Serialize};

        #(#constants)*

        #(#types)*
    };

    Ok(binding.to_string())
}

fn constants(definitions: &[Definition], cxt: &mut Context) -> Result<Vec<TokenStream>, Error> {
    let mut results = vec![];

    for definition in definitions {
        if let Definition::Constant(assign) = definition {
            let identifier = &assign.identifier;
            if let Some(value) = assign.constants::<u32>()? {
                cxt.constants.insert(identifier.to_string(), value);
            }
        }
    }

    for definition in definitions {
        if let Definition::Constant(assign) = definition {
            let (c_name, c_value) = convert_assign::<u32>(assign)?;
            results.push(quote! {
                pub const #c_name: u32 = #c_value;
            });
        }
    }

    Ok(results)
}

fn types(definitions: &[Definition], cxt: &mut Context) -> Result<Vec<TokenStream>, Error> {
    let mut results = vec![];

    for definition in definitions {
        if let Definition::Type(TypeDef::Declaration(declaration)) = definition {
            let (name, value) =
                convert_declaration(declaration, cxt)?.ok_or(Error::NotSupported)?;
            cxt.typedefs.insert(name.to_string(), value);
        }
    }

    for definition in definitions {
        if let Definition::Type(type_def) = definition {
            match type_def {
                TypeDef::Declaration(declaration) => {
                    if !cxt.config.remove_typedef {
                        let (name, value) =
                            convert_declaration(declaration, cxt)?.ok_or(Error::NotSupported)?;
                        let name = upper_camel_case_ident(&name.to_string());
                        results.push(quote! {
                            type #name = #value;
                        });
                    }
                }
                TypeDef::Enum(name, assigns) => {
                    results.push(convert_enum(name, assigns, cxt)?);
                }
                TypeDef::Struct(name, declarations) => {
                    results.push(convert_struct(name, declarations, cxt)?);
                }
                TypeDef::Union(name, body) => {
                    results.push(convert_union(name, body, cxt)?);
                }
            }
        }
    }

    Ok(results)
}

fn convert_assign<'a, T>(assign: &'a Assign) -> Result<(Ident, TokenStream), Error>
where
    T: TryFrom<&'a Constant> + ToTokens,
{
    let name = format_ident!("{}", &assign.identifier);
    let value = convert_value::<T>(&assign.value)?;
    Ok((name, value))
}

fn convert_enum(name: &str, assigns: &[Assign], cxt: &Context) -> Result<TokenStream, Error> {
    let mut values = vec![];

    let mut sindex: i32 = 0;
    for assign in assigns {
        let (e_name, e_value) = convert_assign::<i32>(assign)?;

        let eindex = if let Some(value) = assign.constants::<i32>()? {
            value
        } else {
            let identifier = e_value.to_string();
            let value = cxt
                .constants
                .get(&identifier)
                .ok_or(Error::NotFountIdentifier(identifier))?;
            (*value) as i32
        };

        for index in sindex..eindex {
            // enum は index でシリアライズするため存在しない値は補完する。
            let reserved = format_ident!("_Reserved{}", index.to_string());
            values.push(quote! {
                #reserved = #index
            });
        }

        let e_name = upper_camel_case_ident(&e_name.to_string());
        values.push(quote! {
            #e_name = #e_value
        });

        sindex = eindex + 1;
    }

    let name = upper_camel_case_ident(name);
    Ok(quote! {
        #[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
        #[repr(i32)]
        pub enum #name {
            #(#values),*
        }
    })
}

fn convert_declaration(
    declaration: &Declaration,
    cxt: &Context,
) -> Result<Option<(Ident, TokenStream)>, Error> {
    match declaration {
        Declaration::Variable(type_specifier, name) => {
            let name = format_ident!("{}", name);
            let ty = convert_type(type_specifier, cxt)?;
            Ok(Some((name, ty)))
        }
        Declaration::FixedArray(type_specifier, name, value) => {
            let name = format_ident!("{}", name);
            let ty = convert_type(type_specifier, cxt)?;
            let len = convert_value::<usize>(value)?;
            Ok(Some((name, quote! { [#ty; #len] })))
        }
        Declaration::VariableArray(type_specifier, name, _) => {
            let name = format_ident!("{}", name);
            let ty = convert_type(type_specifier, cxt)?;
            Ok(Some((name, quote! { Vec<#ty> })))
        }
        Declaration::OpaqueFixedArray(name, value) => {
            let name = format_ident!("{}", name);
            let len = convert_value::<usize>(value)?;
            Ok(Some((name, quote! { [u8; #len as usize] })))
        }
        Declaration::OpaqueVariableArray(name, _) => {
            let name = format_ident!("{}", name);
            Ok(Some((name, quote! { Vec<u8> })))
        }
        Declaration::String(name, _) => {
            let name = format_ident!("{}", name);
            Ok(Some((name, quote! { String })))
        }
        Declaration::OptionVariable(type_specifier, name) => {
            let name = format_ident!("{}", name);
            let ty = convert_type(type_specifier, cxt)?;
            Ok(Some((name, quote! { Option<#ty> })))
        }
        Declaration::Void => Ok(None),
    }
}

fn convert_struct(
    name: &str,
    declarations: &[Declaration],
    cxt: &mut Context,
) -> Result<TokenStream, Error> {
    let mut fields = vec![];

    for declaration in declarations {
        let (mut f_name, f_ty) =
            convert_declaration(declaration, cxt)?.ok_or(Error::NotSupported)?;

        if keyword::rust_reserved(&format!("{}", f_name)) {
            f_name = format_ident!("r#{}", f_name);
        }

        let f_name = snake_case_ident(&f_name.to_string());

        fields.push(quote! { pub #f_name: #f_ty });
    }

    let name = upper_camel_case_ident(name);
    Ok(quote! {
        #[derive(Clone, Debug, Deserialize, Serialize)]
        pub struct #name {
            #(#fields),*
        }
    })
}

fn convert_type(specifier: &TypeSpecifier, cxt: &Context) -> Result<TokenStream, Error> {
    match specifier {
        TypeSpecifier::Int(signed) => Ok(if *signed {
            quote! { i32 }
        } else {
            quote! { u32 }
        }),
        TypeSpecifier::Hyper(signed) => Ok(if *signed {
            quote! { i64 }
        } else {
            quote! { u64 }
        }),
        TypeSpecifier::Float => Ok(quote! { f32 }),
        TypeSpecifier::Double => Ok(quote! { f64 }),
        TypeSpecifier::Quadruple => Err(Error::NotSupported),
        TypeSpecifier::Bool => Ok(quote! { bool }),
        TypeSpecifier::Enum(_) => Err(Error::NotSupported),
        TypeSpecifier::Struct(_) => Err(Error::NotSupported),
        TypeSpecifier::Union(_) => Err(Error::NotSupported),
        TypeSpecifier::Identifier(id) => {
            let id = resolve_type(id, cxt)?;
            Ok(quote! { #id })
        }
        TypeSpecifier::Char(signed) => Ok(if *signed {
            quote! { i8 }
        } else {
            quote! { u8 }
        }),
        TypeSpecifier::Short(signed) => Ok(if *signed {
            quote! { i16 }
        } else {
            quote! { u16 }
        }),
    }
}

fn convert_union(name: &str, body: &UnionBody, cxt: &mut Context) -> Result<TokenStream, Error> {
    let mut specs = vec![];

    let cond_type = if let Declaration::Variable(cond_type, _) = &body.cond {
        Ok(cond_type)
    } else {
        Err(Error::NotSupported)
    }?;

    match cond_type {
        TypeSpecifier::Int(_) => {
            let mut sindex: u32 = 0;
            for spec in &body.specs {
                let decl = convert_declaration(&spec.declaration, cxt)?;
                for value in &spec.values {
                    let eindex = match value {
                        Value::Constant(value) => u32::try_from(value)?,
                        Value::Identifier(identifier) => {
                            let value = cxt
                                .constants
                                .get(identifier)
                                .ok_or_else(|| Error::NotFountIdentifier(identifier.to_string()))?;
                            *value
                        }
                    };

                    for index in sindex..eindex {
                        // enum は index でシリアライズするため存在しない値は補完する。
                        let reserved = format_ident!("_Reserved{}", index.to_string());
                        specs.push(quote! {
                            #reserved
                        });
                    }

                    let value = convert_value::<u32>(value)?;
                    let value = upper_camel_case_ident(&value.to_string());
                    if let Some((_, v_ty)) = &decl {
                        specs.push(quote! {
                            #value(#v_ty)
                        });
                    } else {
                        specs.push(quote! {
                            #value
                        });
                    }

                    sindex = eindex + 1;
                }
            }
        }
        TypeSpecifier::Bool => return Err(Error::NotSupported),
        _ => return Err(Error::NotSupported),
    }

    if let Some(default) = &body.default {
        if let Some((_, v_ty)) = convert_declaration(default, cxt)? {
            specs.push(quote! {
                Default(#v_ty)
            });
        } else {
            specs.push(quote! {
                Default
            });
        }
    }

    let name = upper_camel_case_ident(name);
    Ok(quote! {
        #[derive(Clone, Debug, Deserialize, Serialize)]
        pub enum #name {
            #(#specs),*
        }
    })
}

fn convert_value<'a, T>(value: &'a Value) -> Result<TokenStream, Error>
where
    T: TryFrom<&'a Constant> + ToTokens,
{
    match value {
        Value::Constant(c) => {
            let n = T::try_from(c).map_err(|_| Error::Parse(format!("{:?}", c)))?;
            Ok(quote! { #n })
        }
        Value::Identifier(i) => {
            let identifier = format_ident!("{}", i);
            Ok(quote! { #identifier })
        }
    }
}

fn resolve_type(value: &str, cxt: &Context) -> Result<TokenStream, Error> {
    if !cxt.config.remove_typedef {
        let name = upper_camel_case_ident(value);
        return Ok(quote! { #name });
    }

    for (d_name, d_value) in cxt.typedefs.iter() {
        if value == d_name {
            return Ok(quote! { #d_value });
        }
    }

    let name = upper_camel_case_ident(value);
    Ok(quote! { #name })
}

fn upper_camel_case(value: &str) -> String {
    value
        .split('_')
        .map(capitalize)
        .fold("".to_string(), |mut acc, x| {
            acc.push_str(&x);
            acc
        })
}

fn upper_camel_case_ident(value: &str) -> Ident {
    format_ident!("{}", upper_camel_case(value))
}

fn capitalize(value: &str) -> String {
    value
        .chars()
        .enumerate()
        .map(|(index, c)| match index {
            0 => c.to_ascii_uppercase(),
            _ => c.to_ascii_lowercase(),
        })
        .collect()
}

fn snake_case(value: &str) -> String {
    let capitalized = value
        .split_inclusive(char::is_lowercase)
        .map(|c| {
            match c.len() {
                1 => c.to_string(),
                // 複数の大文字が連続する場合は先頭のみ大文字にする。
                // 以下のように変換させる。
                // ex)
                //    userID ->  userId -> user_id
                _ => capitalize(c),
            }
        })
        .fold("".to_string(), |mut acc, x| {
            acc.push_str(&x);
            acc
        });

    let mut v = "".to_string();

    let mut index = 0;
    for (next, _) in capitalized.match_indices(char::is_uppercase) {
        match index {
            0 => v.push_str(&capitalized[index..next].to_ascii_lowercase()),
            _ => v.push_str(&with_underscore(&capitalized[index..next])),
        }

        index = next;
    }

    match index {
        0 => v.push_str(&capitalized.to_ascii_lowercase()),
        _ => v.push_str(&with_underscore(&capitalized[index..])),
    }

    v
}

fn snake_case_ident(value: &str) -> Ident {
    format_ident!("{}", snake_case(value))
}

fn with_underscore(value: &str) -> String {
    let mut v = "_".to_string();
    v.push_str(&value.to_ascii_lowercase());
    v
}

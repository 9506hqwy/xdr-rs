use crate::error::Error;
use crate::keyword;
use crate::parser::{
    Assign, Constant, Declaration, Definition, TypeDef, TypeSpecifier, UnionBody, Value,
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::convert::TryFrom;

#[derive(Clone, Default)]
pub struct Config {
    pub remove_typedef: bool,
    pub complement_enum_index: bool,
    pub enum_impl_indexer: bool,
    pub complement_union_index: bool,
}

struct Context {
    config: Config,
    constants: Vec<(String, Value)>,
    typedefs: Vec<(String, TypeDef)>,
}

impl Context {
    fn new(config: &Config) -> Self {
        Context {
            config: config.clone(),
            constants: vec![],
            typedefs: vec![],
        }
    }

    pub fn count_arms(&self, identifier: &str) -> usize {
        let typedef = self.typedefs.iter().find(|(name, _)| name == identifier);
        if let Some((_, TypeDef::Enum(_, assigns))) = typedef {
            assigns.len()
        } else {
            0
        }
    }

    pub fn is_enum_type(&self, ty: TypeSpecifier) -> bool {
        if let TypeSpecifier::Identifier(name) = ty {
            return self.typedefs.iter().any(|(_, def)| match def {
                TypeDef::Enum(identifier, _) => &name == identifier,
                _ => false,
            });
        }

        false
    }

    pub fn resolve_const_value<'a, T>(&'a self, name: &str) -> Result<T, Error>
    where
        T: TryFrom<&'a Constant>,
    {
        let (_, value) = self
            .constants
            .iter()
            .find(|(n, _)| n == name)
            .ok_or_else(|| Error::NotFountIdentifier(name.to_string()))?;
        match value {
            Value::Constant(c) => c.value::<T>(),
            Value::Identifier(i) => self.resolve_name(i),
        }
    }

    pub fn resolve_enum_value<'a, T>(&'a self, name: &str) -> Result<T, Error>
    where
        T: TryFrom<&'a Constant>,
    {
        for (_, typedef) in &self.typedefs {
            if let TypeDef::Enum(_, assigns) = typedef {
                let assign = assigns.iter().find(|a| a.identifier == name);
                if let Some(assign) = assign {
                    return self.resolve_value::<T>(&assign.value);
                }
            }
        }

        Err(Error::NotFountIdentifier(name.to_string()))
    }

    pub fn resolve_name<'a, T>(&'a self, name: &str) -> Result<T, Error>
    where
        T: TryFrom<&'a Constant>,
    {
        self.resolve_const_value::<T>(name)
            .or_else(|_| self.resolve_enum_value::<T>(name))
    }

    fn resolve_value<'a, T>(&'a self, value: &'a Value) -> Result<T, Error>
    where
        T: TryFrom<&'a Constant>,
    {
        let value = match value {
            Value::Constant(c) => c.value::<T>()?,
            Value::Identifier(identifier) => self.resolve_name::<T>(identifier)?,
        };
        Ok(value)
    }
}

#[derive(Clone, Debug, PartialEq)]
enum OpaqueType {
    None,
    Fixed,
    Variable,
}

pub fn gen(definitions: Vec<Definition>, config: &Config) -> Result<String, Error> {
    let mut cxt = Context::new(config);
    setup_context(&definitions, &mut cxt)?;

    let constants = constants_token(&cxt)?;
    let types = types_token(&cxt)?;

    let use_stmts = if !cxt.config.enum_impl_indexer && cxt.config.complement_union_index {
        quote! {}
    } else {
        quote! {
            use serde::ser::SerializeTuple;
            use serde_xdr::XdrIndexer;
            use serde_xdr_derive::XdrIndexer;
        }
    };

    let binding = quote! {
        use serde::{Deserialize, Serialize};
        #use_stmts

        #(#constants)*

        #(#types)*
    };

    Ok(binding.to_string())
}

fn setup_context(definitions: &[Definition], cxt: &mut Context) -> Result<(), Error> {
    for definition in definitions {
        match definition {
            Definition::Constant(assign) => {
                let identifier = &assign.identifier;
                cxt.constants
                    .push((identifier.to_string(), assign.value.clone()));
            }
            Definition::Type(typedef) => match typedef {
                TypeDef::Declaration(declaration) => {
                    if let Some(name) = declaration.name() {
                        cxt.typedefs.push((name.to_string(), typedef.clone()));
                    }
                }
                TypeDef::Enum(name, _) => {
                    cxt.typedefs.push((name.to_string(), typedef.clone()));
                }
                TypeDef::Struct(name, _) => {
                    cxt.typedefs.push((name.to_string(), typedef.clone()));
                }
                TypeDef::Union(name, _) => {
                    cxt.typedefs.push((name.to_string(), typedef.clone()));
                }
            },
            Definition::LineComment | Definition::Program(_) => {}
        }
    }

    Ok(())
}

fn constants_token(cxt: &Context) -> Result<Vec<TokenStream>, Error> {
    let mut results = vec![];

    for (name, value) in &cxt.constants {
        let name_ident = format_ident!("{}", name);
        let (value_token, ty) = match convert_value_token::<u32>(value, false) {
            Ok(v) => Ok((v, quote! { u32 })),
            Err(_) => match convert_value_token::<u64>(value, false) {
                Ok(v) => Ok((v, quote! { u64 })),
                Err(e) => Err(e),
            },
        }?;
        results.push(quote! {
            pub const #name_ident: #ty = #value_token;
        });
    }

    Ok(results)
}

fn types_token(cxt: &Context) -> Result<Vec<TokenStream>, Error> {
    let mut results = vec![];

    for (name, typedef) in &cxt.typedefs {
        match typedef {
            TypeDef::Declaration(declaration) => {
                if !cxt.config.remove_typedef {
                    let (value, opaque) =
                        convert_type_token(declaration, cxt)?.ok_or(Error::NotSupported)?;
                    if opaque != OpaqueType::None {
                        return Err(Error::NotSupported);
                    }

                    let name_ident = upper_camel_case_ident(name);
                    results.push(quote! {
                        type #name_ident = #value;
                    });
                }
            }
            TypeDef::Enum(_, assigns) => {
                results.push(convert_enum_token(name, assigns, cxt)?);
            }
            TypeDef::Struct(_, declarations) => {
                results.push(convert_struct_token(name, declarations, cxt)?);
            }
            TypeDef::Union(_, body) => {
                results.push(convert_union_token(name, body, cxt)?);
            }
        }
    }

    Ok(results)
}

fn convert_enum_token(name: &str, assigns: &[Assign], cxt: &Context) -> Result<TokenStream, Error> {
    let mut indexed = vec![];
    for assign in assigns {
        let index = cxt.resolve_value::<i32>(&assign.value)?;
        indexed.push((index, assign));
    }

    indexed.sort_by_key(|i| i.0);

    let name_ident = upper_camel_case_ident(name);

    let mut values = vec![];
    let mut as_ref_values = vec![];
    let mut try_from_values = vec![];
    let mut xdr_union_index = vec![];
    let mut xdr_union_name_by_index = vec![];
    let mut default_value = None;

    let mut start: i32 = 0;
    for (end, assign) in indexed {
        if cxt.config.complement_enum_index {
            for index in start..end {
                // enum は index でシリアライズするため存在しない値は補完する。
                let reserved = format_ident!("_Reserved{}", index.to_string());
                values.push(quote! {
                    #reserved = #index
                });
            }
        }

        let item_ident = upper_camel_case_ident(&assign.identifier);
        values.push(quote! {
            #item_ident = #end
        });
        as_ref_values.push(quote! {
            #name_ident::#item_ident => &#end
        });
        try_from_values.push(quote! {
            #end => Ok(#name_ident::#item_ident)
        });
        xdr_union_index.push(quote! {
            #name_ident::#item_ident => #end
        });
        xdr_union_name_by_index.push(quote! {
            #end => Ok(stringify!(#item_ident))
        });

        if default_value.is_none() {
            default_value = Some(quote! { #item_ident });
        }

        start = end + 1;
    }

    let as_ref = if cxt.config.complement_enum_index || cxt.config.enum_impl_indexer {
        quote! {}
    } else {
        quote! {
            impl AsRef<i32> for #name_ident {
                fn as_ref(&self) -> &'static i32 {
                    match self {
                        #(#as_ref_values),*
                    }
                }
            }
        }
    };

    let try_from = if cxt.config.complement_enum_index || cxt.config.enum_impl_indexer {
        quote! {}
    } else {
        quote! {
            impl TryFrom<i32> for #name_ident {
                type Error = serde_xdr::error::Error;

                fn try_from(v: i32) -> Result<Self, serde_xdr::error::Error> {
                    match v {
                        #(#try_from_values),*,
                        _ => Err(serde_xdr::error::Error::Convert),
                    }
                }
            }
        }
    };

    let indexer = if cxt.config.complement_enum_index {
        quote! {}
    } else if cxt.config.enum_impl_indexer {
        let xdr_union_name_by_index_default_arm = match default_value {
            Some(ref v) => quote! { _ => Ok(stringify!(#v)) },
            _ => quote! { _ => Err(::serde_xdr::error::Error::Convert) },
        };

        quote! {
            impl XdrIndexer for #name_ident {
                type Error = ::serde_xdr::error::Error;

                fn name_by_index(index: i32) -> Result<&'static str, Self::Error> {
                    match index {
                        #(#xdr_union_name_by_index),*,
                        #xdr_union_name_by_index_default_arm,
                    }
                }

                fn index(&self) -> i32 {
                    match self {
                        #(#xdr_union_index),*,
                    }
                }
            }
        }
    } else {
        quote! {}
    };

    let derive = if cxt.config.enum_impl_indexer {
        quote! { #[derive(Clone, Debug, PartialEq, XdrIndexer)] }
    } else {
        quote! { #[derive(Clone, Debug, Deserialize, PartialEq, Serialize)] }
    };

    let default_value = default_value.unwrap();
    Ok(quote! {
        #derive
        #[repr(i32)]
        pub enum #name_ident {
            #(#values),*
        }

        impl Default for #name_ident {
            fn default() -> Self {
                #name_ident::#default_value
            }
        }

        #as_ref

        #try_from

        #indexer
    })
}

fn convert_struct_token(
    name: &str,
    declarations: &[Declaration],
    cxt: &Context,
) -> Result<TokenStream, Error> {
    let mut fields = vec![];

    let struct_type = Some(TypeSpecifier::Identifier(name.to_string()));
    for declaration in declarations {
        let mut field_name = declaration.name().unwrap().to_string();
        let (mut ty, opaque) = convert_type_token(declaration, cxt)?.ok_or(Error::NotSupported)?;

        let derive = match opaque {
            OpaqueType::Fixed => quote! { #[serde(with = "serde_xdr::opaque::fixed")] },
            OpaqueType::Variable => quote! { #[serde(with = "serde_xdr::opaque::variable")] },
            _ => {
                if !cxt.config.complement_enum_index
                    && !cxt.config.enum_impl_indexer
                    && cxt.is_enum_type(declaration.type_specifier().unwrap())
                {
                    quote! { #[serde(with = "serde_xdr::primitive::signed32")] }
                } else {
                    quote! {}
                }
            }
        };

        if keyword::rust_reserved(&field_name) {
            field_name = format!("r#{}", field_name);
        }

        let field_name_ident = snake_case_ident(&field_name);

        if struct_type == declaration.type_specifier() {
            // TODO: Add nested structure support.
            ty = quote! { Box<#ty> };
        }

        fields.push(quote! {
            #derive
            pub #field_name_ident: #ty
        });
    }

    let name_ident = upper_camel_case_ident(name);
    Ok(quote! {
        #[derive(Clone, Debug, Default, Deserialize, Serialize)]
        pub struct #name_ident {
            #(#fields),*
        }
    })
}

fn convert_union_token(name: &str, body: &UnionBody, cxt: &Context) -> Result<TokenStream, Error> {
    let mut specs = vec![];
    let mut xdr_union_name_by_index = vec![];
    let mut xdr_union_index = vec![];
    let mut default_value = None;
    let name_ident = upper_camel_case_ident(name);

    let cond_type = match &body.cond {
        Declaration::Variable(cond_type, _) => Ok(cond_type),
        _ => Err(Error::NotSupported),
    }?;

    let arms_count = match cond_type {
        TypeSpecifier::Identifier(identifier) => cxt.count_arms(identifier),
        TypeSpecifier::Bool => 2,
        _ => 0,
    };

    match cond_type {
        TypeSpecifier::Int(_) | TypeSpecifier::Identifier(_) => {
            let mut values = vec![];
            for spec in &body.specs {
                for value in &spec.values {
                    let index = cxt.resolve_value(value)?;
                    values.push((index, spec.declaration.clone(), value));
                }
            }

            values.sort_by_key(|s| s.0);

            let mut sindex: u32 = 0;
            for (eindex, decl, value) in values {
                if cxt.config.complement_union_index {
                    for index in sindex..eindex {
                        // enum は index でシリアライズするため存在しない値は補完する。
                        let reserved = format_ident!("_Reserved{}", index.to_string());
                        specs.push(quote! {
                            #reserved
                        });
                    }
                }

                let (variant, variant_arm, value, default) = convert_case_token(value, &decl, cxt)?;
                specs.push(value);
                if default_value.is_none() {
                    default_value = Some(default);
                }

                let index = eindex as i32;
                xdr_union_name_by_index.push(quote! {
                    #index => Ok(stringify!(#variant))
                });

                xdr_union_index.push(quote! {
                    #name_ident::#variant_arm => #index
                });

                sindex = eindex + 1;
            }
        }
        TypeSpecifier::Bool => {
            let false_case = body
                .specs
                .iter()
                .find(|s| s.values.iter().any(|v| v.is_false()));
            match false_case {
                Some(case) => {
                    let value = case.values.iter().find(|v| v.is_false()).unwrap();

                    let (variant, variant_arm, value, default) =
                        convert_case_token(value, &case.declaration, cxt)?;
                    specs.push(value);
                    if default_value.is_none() {
                        default_value = Some(default);
                    }

                    xdr_union_name_by_index.push(quote! {
                        0i32 => Ok(stringify!(#variant))
                    });

                    xdr_union_index.push(quote! {
                        #name_ident::#variant_arm => 0i32
                    });
                }
                _ => {
                    if cxt.config.complement_union_index {
                        specs.push(quote! {
                            _Reserved0
                        });
                    }
                }
            }

            let true_case = body
                .specs
                .iter()
                .find(|s| s.values.iter().any(|v| v.is_true()));
            match true_case {
                Some(case) => {
                    let value = case.values.iter().find(|v| v.is_true()).unwrap();

                    let (variant, variant_arm, value, default) =
                        convert_case_token(value, &case.declaration, cxt)?;
                    specs.push(value);
                    if default_value.is_none() {
                        default_value = Some(default);
                    }

                    xdr_union_name_by_index.push(quote! {
                        1i32 => Ok(stringify!(#variant))
                    });

                    xdr_union_index.push(quote! {
                        #name_ident::#variant_arm => 1i32
                    });
                }
                _ => {
                    if cxt.config.complement_union_index {
                        specs.push(quote! {
                            _Reserved1
                        });
                    }
                }
            }
        }
        _ => return Err(Error::NotSupported),
    }

    if let Some(default) = &body.default {
        if let Some((v_ty, _)) = convert_type_token(default, cxt)? {
            specs.push(quote! {
                Default(#v_ty)
            });

            default_value = Some(quote! { Default(Default::default()) });
        } else {
            specs.push(quote! {
                Default
            });

            default_value = Some(quote! { Default });
        }
    }

    let xdr_union = if cxt.config.complement_union_index {
        quote! {}
    } else {
        let xdr_union_name_by_index_default_arm = if body.default.is_some() {
            quote! { _ => Ok(stringify!(Default)) }
        } else {
            quote! { _ => Err(::serde_xdr::error::Error::Convert) }
        };

        let xdr_union_index_default_arm = if arms_count == xdr_union_index.len() {
            quote! {}
        } else {
            quote! { _ => unimplemented!() }
        };

        quote! {
            impl XdrIndexer for #name_ident {
                type Error = ::serde_xdr::error::Error;

                fn name_by_index(index: i32) -> Result<&'static str, Self::Error> {
                    match index {
                        #(#xdr_union_name_by_index),*,
                        #xdr_union_name_by_index_default_arm,
                    }
                }

                fn index(&self) -> i32 {
                    match self {
                        #(#xdr_union_index),*,
                        #xdr_union_index_default_arm
                    }
                }
            }
        }
    };

    let derive = if cxt.config.complement_union_index {
        quote! { #[derive(Clone, Debug, Deserialize, Serialize)] }
    } else {
        quote! { #[derive(Clone, Debug, XdrIndexer)] }
    };

    let default_value = default_value.unwrap();
    Ok(quote! {
        #derive
        pub enum #name_ident {
            #(#specs),*
        }

        impl Default for #name_ident {
            fn default() -> Self {
                #name_ident::#default_value
            }
        }

        #xdr_union
    })
}

fn convert_value_token<'a, T>(value: &'a Value, cnv: bool) -> Result<TokenStream, Error>
where
    T: TryFrom<&'a Constant> + ToTokens,
{
    match value {
        Value::Constant(c) => {
            let v = c.value::<T>()?;
            Ok(quote! { #v })
        }
        Value::Identifier(i) => {
            let v = if cnv {
                upper_camel_case_ident(i)
            } else {
                format_ident!("{}", i)
            };
            Ok(quote! { #v })
        }
    }
}

fn convert_type_token(
    declaration: &Declaration,
    cxt: &Context,
) -> Result<Option<(TokenStream, OpaqueType)>, Error> {
    match declaration {
        Declaration::Variable(type_specifier, _) => {
            let (ty, opaque) = convert_primitive_token(type_specifier, cxt)?;
            Ok(Some((ty, opaque)))
        }
        Declaration::FixedArray(type_specifier, _, value) => {
            let (ty, opaque) = convert_primitive_token(type_specifier, cxt)?;
            let len = convert_value_token::<usize>(value, false)?;
            Ok(Some((quote! { [#ty; #len] }, opaque)))
        }
        Declaration::VariableArray(type_specifier, _, _) => {
            let (ty, opaque) = convert_primitive_token(type_specifier, cxt)?;
            if opaque == OpaqueType::Variable {
                Ok(Some((
                    quote! { Vec<serde_xdr::opaque::VariableArray> },
                    OpaqueType::None,
                )))
            } else {
                // TODO: Add inner fixed opaque array support.
                Ok(Some((quote! { Vec<#ty> }, opaque)))
            }
        }
        Declaration::OpaqueFixedArray(_, value) => {
            let len = convert_value_token::<usize>(value, false)?;
            Ok(Some((quote! { [u8; #len as usize] }, OpaqueType::Fixed)))
        }
        Declaration::OpaqueVariableArray(_, _) => {
            Ok(Some((quote! { Vec<u8> }, OpaqueType::Variable)))
        }
        Declaration::String(_, _) => Ok(Some((quote! { String }, OpaqueType::None))),
        Declaration::OptionVariable(type_specifier, _) => {
            let (ty, opaque) = convert_primitive_token(type_specifier, cxt)?;
            Ok(Some((quote! { Option<#ty> }, opaque)))
        }
        Declaration::Void => Ok(None),
    }
}

fn convert_primitive_token(
    specifier: &TypeSpecifier,
    cxt: &Context,
) -> Result<(TokenStream, OpaqueType), Error> {
    match specifier {
        TypeSpecifier::Int(signed) => Ok(if *signed {
            (quote! { i32 }, OpaqueType::None)
        } else {
            (quote! { u32 }, OpaqueType::None)
        }),
        TypeSpecifier::Hyper(signed) => Ok(if *signed {
            (quote! { i64 }, OpaqueType::None)
        } else {
            (quote! { u64 }, OpaqueType::None)
        }),
        TypeSpecifier::Float => Ok((quote! { f32 }, OpaqueType::None)),
        TypeSpecifier::Double => Ok((quote! { f64 }, OpaqueType::None)),
        TypeSpecifier::Quadruple => Err(Error::NotSupported),
        TypeSpecifier::Bool => Ok((quote! { bool }, OpaqueType::None)),
        TypeSpecifier::Enum(_) => Err(Error::NotSupported),
        TypeSpecifier::Struct(_) => Err(Error::NotSupported),
        TypeSpecifier::Union(_) => Err(Error::NotSupported),
        TypeSpecifier::Identifier(id) => {
            let (id, opaque) = resolve_type_token(id, cxt)?;
            Ok((quote! { #id }, opaque))
        }
        TypeSpecifier::Char(signed) => Ok(if *signed {
            (quote! { i8 }, OpaqueType::None)
        } else {
            (quote! { u8 }, OpaqueType::None)
        }),
        TypeSpecifier::Short(signed) => Ok(if *signed {
            (quote! { i16 }, OpaqueType::None)
        } else {
            (quote! { u16 }, OpaqueType::None)
        }),
        TypeSpecifier::Void => unimplemented!(),
    }
}

fn convert_case_token(
    value: &Value,
    declaration: &Declaration,
    cxt: &Context,
) -> Result<(TokenStream, TokenStream, TokenStream, TokenStream), Error> {
    let value = convert_value_token::<u32>(value, true)?;

    let decl = convert_type_token(declaration, cxt)?;
    if let Some((mut v_ty, opaque)) = decl {
        let derive = if opaque != OpaqueType::None && !cxt.config.complement_union_index {
            // XdrUnion を使用する場合は serde 属性が使用できないのでラッパオブジェクトを使用する。
            v_ty = quote! { serde_xdr::opaque::VariableArray };
            quote! {}
        } else {
            match opaque {
                OpaqueType::Fixed => quote! { #[serde(with = "serde_xdr::opaque::fixed")] },
                OpaqueType::Variable => quote! { #[serde(with = "serde_xdr::opaque::variable")] },
                _ => quote! {},
            }
        };

        let value_token = quote! {
            #derive
            #value(#v_ty)
        };
        let default_token = quote! { #value(Default::default()) };
        Ok((
            value.clone(),
            quote! { #value(_) },
            value_token,
            default_token,
        ))
    } else {
        let value_token = quote! { #value };
        let default_token = quote! { #value };
        Ok((value.clone(), value, value_token, default_token))
    }
}

fn resolve_type_token(value: &str, cxt: &Context) -> Result<(TokenStream, OpaqueType), Error> {
    if !cxt.config.remove_typedef {
        let name = upper_camel_case_ident(value);
        return Ok((quote! { #name }, OpaqueType::None));
    }

    let typedef = cxt.typedefs.iter().find(|(name, _)| name == value);
    if let Some((_, TypeDef::Declaration(declaration))) = typedef {
        let (value, opaque) = convert_type_token(declaration, cxt)?.ok_or(Error::NotSupported)?;

        return Ok((quote! { #value }, opaque));
    }

    let name = upper_camel_case_ident(value);
    Ok((quote! { #name }, OpaqueType::None))
}

fn upper_camel_case(value: &str) -> String {
    if value.contains('_') {
        value
            .split('_')
            .map(capitalize)
            .fold("".to_string(), |mut acc, x| {
                acc.push_str(&x);
                acc
            })
    } else {
        value
            .chars()
            .enumerate()
            .map(|(index, c)| match index {
                0 => c.to_ascii_uppercase(),
                _ => c,
            })
            .collect()
    }
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
        .split_inclusive(|c| char::is_lowercase(c) || char::is_numeric(c))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn context_resolve_constant_value_ok() {
        let mut cxt = Context::new(&Default::default());
        cxt.constants.push((
            "a".to_string(),
            Value::Constant(Constant::Decimal("10".to_string())),
        ));
        let n = cxt.resolve_const_value::<u32>("a").unwrap();
        assert_eq!(10u32, n);
    }

    #[test]
    fn context_resolve_constant_value_err() {
        let mut cxt = Context::new(&Default::default());
        cxt.constants.push((
            "a".to_string(),
            Value::Constant(Constant::Decimal("10".to_string())),
        ));
        let n = cxt.resolve_const_value::<u32>("n");
        assert!(n.is_err());
    }

    #[test]
    fn context_resolve_enum_value_ok() {
        let mut cxt = Context::new(&Default::default());
        cxt.typedefs.push((
            "a".to_string(),
            TypeDef::Enum(
                "a".to_string(),
                vec![Assign {
                    identifier: "b".to_string(),
                    value: Value::Constant(Constant::Decimal("10".to_string())),
                }],
            ),
        ));
        let n = cxt.resolve_enum_value::<u32>("b").unwrap();
        assert_eq!(10u32, n);
    }

    #[test]
    fn context_resolve_enum_value_err() {
        let mut cxt = Context::new(&Default::default());
        cxt.typedefs.push((
            "a".to_string(),
            TypeDef::Enum(
                "a".to_string(),
                vec![Assign {
                    identifier: "b".to_string(),
                    value: Value::Constant(Constant::Decimal("10".to_string())),
                }],
            ),
        ));
        let n = cxt.resolve_enum_value::<u32>("n");
        assert!(n.is_err());
    }

    #[test]
    fn context_resolve_name_ok1() {
        let mut cxt = Context::new(&Default::default());
        cxt.constants.push((
            "a".to_string(),
            Value::Constant(Constant::Decimal("10".to_string())),
        ));
        cxt.typedefs.push((
            "b".to_string(),
            TypeDef::Enum(
                "b".to_string(),
                vec![Assign {
                    identifier: "c".to_string(),
                    value: Value::Constant(Constant::Decimal("11".to_string())),
                }],
            ),
        ));
        let n = cxt.resolve_name::<u32>("a").unwrap();
        assert_eq!(10u32, n);
    }

    #[test]
    fn context_resolve_name_ok2() {
        let mut cxt = Context::new(&Default::default());
        cxt.constants.push((
            "a".to_string(),
            Value::Constant(Constant::Decimal("10".to_string())),
        ));
        cxt.typedefs.push((
            "b".to_string(),
            TypeDef::Enum(
                "b".to_string(),
                vec![Assign {
                    identifier: "c".to_string(),
                    value: Value::Constant(Constant::Decimal("11".to_string())),
                }],
            ),
        ));
        let n = cxt.resolve_name::<u32>("c").unwrap();
        assert_eq!(11u32, n);
    }

    #[test]
    fn upper_camel_case_to_upper_camel_case() {
        let r = upper_camel_case("AbCd");
        assert_eq!("AbCd", r)
    }

    #[test]
    fn lower_camel_case_to_upper_camel_case() {
        let r = upper_camel_case("abCd");
        assert_eq!("AbCd", r)
    }

    #[test]
    fn snake_case_to_upper_camel_case() {
        let r = upper_camel_case("ab_cd");
        assert_eq!("AbCd", r)
    }

    #[test]
    fn upper_snake_case_to_upper_camel_case() {
        let r = upper_camel_case("AB_CD");
        assert_eq!("AbCd", r)
    }

    #[test]
    fn upper_camel_case_to_snake_case() {
        let r = snake_case("AbCd");
        assert_eq!("ab_cd", r)
    }

    #[test]
    fn lower_camel_case_to_snake_case() {
        let r = snake_case("abCd");
        assert_eq!("ab_cd", r)
    }

    #[test]
    fn snake_case_to_snake_case() {
        let r = snake_case("ab_cd");
        assert_eq!("ab_cd", r)
    }

    #[test]
    fn upper_camel_case_to_snake_case_with_num() {
        let r = snake_case("Ab2Cd");
        assert_eq!("ab2_cd", r)
    }
}

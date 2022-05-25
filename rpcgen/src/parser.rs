use crate::error::Error;
use crate::keyword;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{
        alpha1, alphanumeric1, digit1, hex_digit1, line_ending, multispace0, multispace1,
        not_line_ending, oct_digit1, one_of,
    },
    combinator::{map, opt, peek, recognize, verify},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::convert::TryFrom;
use std::str::FromStr;

pub fn parse(input: &str) -> Result<Vec<Definition>, Error> {
    let (rest, ret) = specification(input)?;
    if !rest.is_empty() {
        return Err(Error::TrailingData);
    }
    Ok(ret)
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assign {
    pub identifier: String,
    pub value: Value,
}

impl Assign {
    fn new(identifier: &str, value: Value) -> Self {
        Assign {
            identifier: identifier.to_string(),
            value,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CaseSpec {
    pub values: Vec<Value>,
    pub declaration: Declaration,
}

impl CaseSpec {
    fn new(values: Vec<Value>, declaration: Declaration) -> Self {
        CaseSpec {
            values,
            declaration,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    Decimal(String),
    Hex(String),
    Octal(String),
}

impl<'a> Constant {
    pub fn value<T>(&'a self) -> Result<T, Error>
    where
        T: TryFrom<&'a Constant>,
    {
        let n = T::try_from(self).map_err(|_| Error::Parse(format!("{:?}", self)))?;
        Ok(n)
    }
}

impl TryFrom<&Constant> for i32 {
    type Error = std::num::ParseIntError;

    fn try_from(value: &Constant) -> Result<Self, Self::Error> {
        match value {
            Constant::Decimal(v) => i32::from_str(v),
            Constant::Hex(v) => i32::from_str_radix(v, 16),
            Constant::Octal(v) => i32::from_str_radix(v, 8),
        }
    }
}

impl TryFrom<&Constant> for u32 {
    type Error = std::num::ParseIntError;

    fn try_from(value: &Constant) -> Result<Self, Self::Error> {
        match value {
            Constant::Decimal(v) => u32::from_str(v),
            Constant::Hex(v) => u32::from_str_radix(v, 16),
            Constant::Octal(v) => u32::from_str_radix(v, 8),
        }
    }
}

impl TryFrom<&Constant> for u64 {
    type Error = std::num::ParseIntError;

    fn try_from(value: &Constant) -> Result<Self, Self::Error> {
        match value {
            Constant::Decimal(v) => u64::from_str(v),
            Constant::Hex(v) => u64::from_str_radix(v, 16),
            Constant::Octal(v) => u64::from_str_radix(v, 8),
        }
    }
}

impl TryFrom<&Constant> for usize {
    type Error = std::num::ParseIntError;

    fn try_from(value: &Constant) -> Result<Self, Self::Error> {
        match value {
            Constant::Decimal(v) => usize::from_str(v),
            Constant::Hex(v) => usize::from_str_radix(v, 16),
            Constant::Octal(v) => usize::from_str_radix(v, 8),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    Variable(TypeSpecifier, String),
    FixedArray(TypeSpecifier, String, Value),
    VariableArray(TypeSpecifier, String, Option<Value>),
    OpaqueFixedArray(String, Value),
    OpaqueVariableArray(String, Option<Value>),
    String(String, Option<Value>),
    OptionVariable(TypeSpecifier, String),
    Void,
}

impl Declaration {
    fn fixed_array(type_specifier: TypeSpecifier, identifier: &str, value: Value) -> Self {
        Declaration::FixedArray(type_specifier, identifier.to_string(), value)
    }

    fn opaque_fixed_array(identifier: &str, value: Value) -> Self {
        Declaration::OpaqueFixedArray(identifier.to_string(), value)
    }

    fn opaque_variable_array(identifier: &str, value: Option<Value>) -> Self {
        Declaration::OpaqueVariableArray(identifier.to_string(), value)
    }

    fn option_variable(type_specifier: TypeSpecifier, identifier: &str) -> Self {
        Declaration::OptionVariable(type_specifier, identifier.to_string())
    }

    fn string(identifier: &str, value: Option<Value>) -> Self {
        Declaration::String(identifier.to_string(), value)
    }

    fn variable(type_specifier: TypeSpecifier, identifier: &str) -> Self {
        Declaration::Variable(type_specifier, identifier.to_string())
    }

    fn variable_array(
        type_specifier: TypeSpecifier,
        identifier: &str,
        value: Option<Value>,
    ) -> Self {
        Declaration::VariableArray(type_specifier, identifier.to_string(), value)
    }

    fn void() -> Self {
        Declaration::Void
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            Declaration::Variable(_, name) => Some(name),
            Declaration::FixedArray(_, name, _) => Some(name),
            Declaration::VariableArray(_, name, _) => Some(name),
            Declaration::OpaqueFixedArray(name, _) => Some(name),
            Declaration::OpaqueVariableArray(name, _) => Some(name),
            Declaration::String(name, _) => Some(name),
            Declaration::OptionVariable(_, name) => Some(name),
            Declaration::Void => None,
        }
    }

    pub fn type_specifier(&self) -> Option<TypeSpecifier> {
        match self {
            Declaration::Variable(ty, _) => Some(ty.clone()),
            Declaration::FixedArray(ty, _, _) => Some(ty.clone()),
            Declaration::VariableArray(ty, _, _) => Some(ty.clone()),
            Declaration::OpaqueFixedArray(_, _) => Some(TypeSpecifier::identifier_type("u8")),
            Declaration::OpaqueVariableArray(_, _) => Some(TypeSpecifier::identifier_type("u8")),
            Declaration::String(_, _) => Some(TypeSpecifier::identifier_type("String")),
            Declaration::OptionVariable(ty, _) => Some(ty.clone()),
            Declaration::Void => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Definition {
    Type(TypeDef),
    Constant(Assign),
    LineComment,
    // start (rpcl extension)
    Program(Program),
    // end (rpcl extension)
}

impl Definition {
    fn type_def(def: TypeDef) -> Self {
        Definition::Type(def)
    }

    fn constant_def(def: Assign) -> Self {
        Definition::Constant(def)
    }

    fn linecomment_def() -> Self {
        Definition::LineComment
    }

    fn program(prog: Program) -> Self {
        Definition::Program(prog)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumTypeSpec {
    pub body: Vec<Assign>,
}

impl EnumTypeSpec {
    fn new(body: Vec<Assign>) -> Self {
        EnumTypeSpec { body }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructTypeSpec {
    pub body: Vec<Declaration>,
}

impl StructTypeSpec {
    fn new(body: Vec<Declaration>) -> Self {
        StructTypeSpec { body }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeDef {
    Declaration(Declaration),
    Enum(String, Vec<Assign>),
    Struct(String, Vec<Declaration>),
    Union(String, UnionBody),
}

impl TypeDef {
    fn declaration_def(declaration: Declaration) -> Self {
        TypeDef::Declaration(declaration)
    }

    fn enum_def(identifier: &str, assigns: Vec<Assign>) -> Self {
        TypeDef::Enum(identifier.to_string(), assigns)
    }

    fn struct_def(identifier: &str, declarations: Vec<Declaration>) -> Self {
        TypeDef::Struct(identifier.to_string(), declarations)
    }

    fn union_def(identifier: &str, body: UnionBody) -> Self {
        TypeDef::Union(identifier.to_string(), body)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeSpecifier {
    Int(bool),
    Hyper(bool),
    Float,
    Double,
    Quadruple,
    Bool,
    Enum(Box<EnumTypeSpec>),
    Struct(Box<StructTypeSpec>),
    Union(Box<UnionTypeSpec>),
    Identifier(String),
    // start (libvirt extenstion)
    Char(bool),
    Short(bool),
    // end (libvirt extenstion)
    // start (rpcl extension)
    Void,
    // end (rpcl extension)
}

impl TypeSpecifier {
    fn enum_type(spec: EnumTypeSpec) -> Self {
        TypeSpecifier::Enum(Box::new(spec))
    }

    fn identifier_type(identifier: &str) -> Self {
        TypeSpecifier::Identifier(identifier.to_string())
    }

    fn struct_type(spec: StructTypeSpec) -> Self {
        TypeSpecifier::Struct(Box::new(spec))
    }

    fn union_type(spec: UnionTypeSpec) -> Self {
        TypeSpecifier::Union(Box::new(spec))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnionBody {
    pub cond: Declaration,
    pub specs: Vec<CaseSpec>,
    pub default: Option<Declaration>,
}

impl UnionBody {
    fn new(cond: Declaration, specs: Vec<CaseSpec>, default: Option<Declaration>) -> UnionBody {
        UnionBody {
            cond,
            specs,
            default,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnionTypeSpec {
    pub body: UnionBody,
}

impl UnionTypeSpec {
    fn new(body: UnionBody) -> Self {
        UnionTypeSpec { body }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Constant(Constant),
    Identifier(String),
}

impl Value {
    fn constant(value: Constant) -> Self {
        Value::Constant(value)
    }

    fn identifier(value: &str) -> Self {
        Value::Identifier(value.to_string())
    }

    pub fn constants<'a, T>(&'a self) -> Result<Option<T>, Error>
    where
        T: TryFrom<&'a Constant>,
    {
        match self {
            Value::Constant(c) => Ok(Some(c.value::<T>()?)),
            _ => Ok(None),
        }
    }

    pub fn is_false(&self) -> bool {
        if let Value::Identifier(value) = &self {
            value == "FALSE"
        } else {
            false
        }
    }

    pub fn is_true(&self) -> bool {
        if let Value::Identifier(value) = &self {
            value == "TRUE"
        } else {
            false
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub name: String,
    pub versions: Vec<Version>,
    pub value: Value,
}

impl Program {
    fn new(name: &str, versions: Vec<Version>, value: Value) -> Self {
        Program {
            name: name.to_string(),
            versions,
            value,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Version {
    pub name: String,
    pub procedures: Vec<Procedure>,
    pub value: Value,
}

impl Version {
    fn new(name: &str, procedures: Vec<Procedure>, value: Value) -> Self {
        Version {
            name: name.to_string(),
            procedures,
            value,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Procedure {
    pub res_ty: TypeSpecifier,
    pub method: String,
    pub req_ty: TypeSpecifier,
    pub value: Value,
}

impl Procedure {
    fn new(res_ty: TypeSpecifier, method: &str, req_ty: TypeSpecifier, value: Value) -> Self {
        Procedure {
            res_ty,
            method: method.to_string(),
            req_ty,
            value,
        }
    }
}

fn assign(input: &str) -> IResult<&str, Assign> {
    map(
        tuple((
            identifier,
            preceded(tuple((comment0, tag("="), comment0)), value),
        )),
        |(i, v)| Assign::new(i, v),
    )(input)
}

fn case_spec(input: &str) -> IResult<&str, CaseSpec> {
    map(
        tuple((
            many1(delimited(
                tuple((comment0, tag("case"), comment0)),
                value,
                tuple((comment0, tag(":"), comment0)),
            )),
            terminated(declaration, tuple((comment0, tag(";")))),
        )),
        |(v, d)| CaseSpec::new(v, d),
    )(input)
}

fn comment0(input: &str) -> IResult<&str, Option<Vec<&str>>> {
    preceded(
        multispace0,
        opt(many1(terminated(
            delimited(tag("/*"), take_until("*/"), tag("*/")),
            multispace0,
        ))),
    )(input)
}

fn comment_line(input: &str) -> IResult<&str, &str> {
    delimited(tag("%"), not_line_ending, line_ending)(input)
}

fn constant(input: &str) -> IResult<&str, Constant> {
    alt((
        decimal_nonzero,
        hexadecimal,
        octal,
        map(tag("0"), |v: &str| Constant::Decimal(v.to_string())),
    ))(input)
}

fn constant_def(input: &str) -> IResult<&str, Assign> {
    preceded(
        tuple((tag("const"), multispace1)),
        terminated(assign, tuple((comment0, tag(";")))),
    )(input)
}

fn decimal_nonzero(input: &str) -> IResult<&str, Constant> {
    map(
        recognize(tuple((opt(tag("-")), one_of("1234567879"), many0(digit1)))),
        |v: &str| Constant::Decimal(v.to_string()),
    )(input)
}

fn declaration(input: &str) -> IResult<&str, Declaration> {
    // 優先度順に並べる。
    alt((
        map(tag("void"), |_| Declaration::void()),
        map(
            tuple((
                preceded(tuple((tag("opaque"), multispace1)), identifier),
                delimited(tag("["), value, tag("]")),
            )),
            |(i, v)| Declaration::opaque_fixed_array(i, v),
        ),
        map(
            tuple((
                preceded(tuple((tag("opaque"), multispace1)), identifier),
                delimited(tag("<"), opt(value), tag(">")),
            )),
            |(i, v)| Declaration::opaque_variable_array(i, v),
        ),
        map(
            tuple((
                preceded(tuple((tag("string"), multispace1)), identifier),
                delimited(tag("<"), opt(value), tag(">")),
            )),
            |(i, v)| Declaration::string(i, v),
        ),
        map(
            tuple((
                type_specifier,
                preceded(multispace1, identifier),
                delimited(tag("["), value, tag("]")),
            )),
            |(t, i, v)| Declaration::fixed_array(t, i, v),
        ),
        map(
            tuple((
                type_specifier,
                preceded(multispace1, identifier),
                delimited(tag("<"), opt(value), tag(">")),
            )),
            |(t, i, v)| Declaration::variable_array(t, i, v),
        ),
        map(
            tuple((
                type_specifier,
                preceded(tuple((multispace0, tag("*"), multispace0)), identifier),
            )),
            |(t, i)| Declaration::option_variable(t, i),
        ),
        map(
            tuple((type_specifier, preceded(multispace1, identifier))),
            |(t, i)| Declaration::variable(t, i),
        ),
    ))(input)
}

fn definition(input: &str) -> IResult<&str, Definition> {
    alt((
        map(constant_def, Definition::constant_def),
        map(type_def, Definition::type_def),
        map(program, Definition::program),
        map(comment_line, |_| Definition::linecomment_def()),
    ))(input)
}

fn enum_body(input: &str) -> IResult<&str, Vec<Assign>> {
    delimited(
        tuple((tag("{"), comment0)),
        separated_list1(tuple((comment0, tag(","), comment0)), assign),
        tuple((comment0, tag("}"))),
    )(input)
}

fn enum_type_spec(input: &str) -> IResult<&str, EnumTypeSpec> {
    map(
        preceded(tuple((tag("enum"), multispace1)), enum_body),
        EnumTypeSpec::new,
    )(input)
}

fn hexadecimal(input: &str) -> IResult<&str, Constant> {
    map(
        preceded(tag("0x"), recognize(many1(hex_digit1))),
        |v: &str| Constant::Hex(v.to_string()),
    )(input)
}

fn identifier(input: &str) -> IResult<&str, &str> {
    verify(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |i| !keyword::xdr_reserved(i),
    )(input)
}

fn octal(input: &str) -> IResult<&str, Constant> {
    map(
        preceded(tag("0"), recognize(many1(oct_digit1))),
        |v: &str| Constant::Octal(v.to_string()),
    )(input)
}

fn specification(input: &str) -> IResult<&str, Vec<Definition>> {
    many0(delimited(comment0, definition, comment0))(input)
}

fn struct_body(input: &str) -> IResult<&str, Vec<Declaration>> {
    delimited(
        tuple((tag("{"), comment0)),
        many1(terminated(
            declaration,
            tuple((comment0, tag(";"), comment0)),
        )),
        tuple((comment0, tag("}"))),
    )(input)
}

fn struct_type_spec(input: &str) -> IResult<&str, StructTypeSpec> {
    map(
        preceded(tuple((tag("struct"), multispace1)), struct_body),
        StructTypeSpec::new,
    )(input)
}

fn type_def(input: &str) -> IResult<&str, TypeDef> {
    alt((
        map(
            delimited(
                tuple((tag("typedef"), multispace1)),
                declaration,
                tuple((comment0, tag(";"))),
            ),
            TypeDef::declaration_def,
        ),
        map(
            delimited(
                tuple((tag("enum"), multispace1)),
                tuple((terminated(identifier, comment0), enum_body)),
                tuple((comment0, tag(";"))),
            ),
            |(i, v)| TypeDef::enum_def(i, v),
        ),
        map(
            delimited(
                tuple((tag("struct"), multispace1)),
                tuple((terminated(identifier, comment0), struct_body)),
                tuple((comment0, tag(";"))),
            ),
            |(i, v)| TypeDef::struct_def(i, v),
        ),
        map(
            delimited(
                tuple((tag("union"), multispace1)),
                tuple((terminated(identifier, comment0), union_body)),
                tuple((comment0, tag(";"))),
            ),
            |(i, v)| TypeDef::union_def(i, v),
        ),
    ))(input)
}

fn type_specifier(input: &str) -> IResult<&str, TypeSpecifier> {
    alt((
        map(
            tuple((
                tag("unsigned"),
                multispace1,
                terminated(tag("int"), peek(multispace1)),
            )),
            |_| TypeSpecifier::Int(false),
        ),
        map(terminated(tag("int"), peek(multispace1)), |_| {
            TypeSpecifier::Int(true)
        }),
        map(
            tuple((
                tag("unsigned"),
                multispace1,
                terminated(tag("hyper"), peek(multispace1)),
            )),
            |_| TypeSpecifier::Hyper(false),
        ),
        map(terminated(tag("hyper"), peek(multispace1)), |_| {
            TypeSpecifier::Hyper(true)
        }),
        // start (libvirt extenstion)
        map(
            tuple((
                tag("unsigned"),
                multispace1,
                terminated(tag("char"), peek(multispace1)),
            )),
            |_| TypeSpecifier::Char(false),
        ),
        map(terminated(tag("char"), peek(multispace1)), |_| {
            TypeSpecifier::Char(true)
        }),
        map(
            tuple((
                tag("unsigned"),
                multispace1,
                terminated(tag("short"), peek(multispace1)),
            )),
            |_| TypeSpecifier::Short(false),
        ),
        map(terminated(tag("short"), peek(multispace1)), |_| {
            TypeSpecifier::Short(true)
        }),
        map(terminated(tag("unsigned"), peek(multispace1)), |_| {
            TypeSpecifier::Int(false)
        }),
        // end (libvirt extenstion)
        map(terminated(tag("float"), peek(multispace1)), |_| {
            TypeSpecifier::Float
        }),
        map(terminated(tag("double"), peek(multispace1)), |_| {
            TypeSpecifier::Double
        }),
        map(terminated(tag("quadruple"), peek(multispace1)), |_| {
            TypeSpecifier::Quadruple
        }),
        map(terminated(tag("bool"), peek(multispace1)), |_| {
            TypeSpecifier::Bool
        }),
        map(enum_type_spec, TypeSpecifier::enum_type),
        map(struct_type_spec, TypeSpecifier::struct_type),
        map(union_type_spec, TypeSpecifier::union_type),
        map(identifier, TypeSpecifier::identifier_type),
    ))(input)
}

fn union_body(input: &str) -> IResult<&str, UnionBody> {
    map(
        tuple((
            delimited(
                tuple((tag("switch"), comment0, tag("("), comment0)),
                declaration,
                tuple((comment0, tag(")"), comment0, tag("{"), comment0)),
            ),
            many1(terminated(case_spec, comment0)),
            terminated(
                opt(delimited(
                    tuple((comment0, tag("default"), comment0, tag(":"), comment0)),
                    declaration,
                    tuple((comment0, tag(";"))),
                )),
                tuple((comment0, tag("}"))),
            ),
        )),
        |(c, s, d)| UnionBody::new(c, s, d),
    )(input)
}

fn union_type_spec(input: &str) -> IResult<&str, UnionTypeSpec> {
    map(
        preceded(tuple((tag("union"), multispace1)), union_body),
        UnionTypeSpec::new,
    )(input)
}

fn value(input: &str) -> IResult<&str, Value> {
    alt((
        map(constant, Value::constant),
        map(identifier, Value::identifier),
    ))(input)
}

fn program(input: &str) -> IResult<&str, Program> {
    map(
        tuple((
            preceded(tuple((tag("program"), multispace1)), identifier),
            delimited(
                tuple((multispace0, tag("{"), multispace0)),
                many1(version),
                tuple((multispace0, tag("}"), multispace0)),
            ),
            delimited(
                tuple((tag("="), multispace0)),
                value,
                tuple((multispace0, tag(";"))),
            ),
        )),
        |(a, b, c)| Program::new(a, b, c),
    )(input)
}

fn version(input: &str) -> IResult<&str, Version> {
    map(
        tuple((
            preceded(tuple((tag("version"), multispace1)), identifier),
            delimited(
                tuple((multispace0, tag("{"), multispace0)),
                many1(terminated(procedure, multispace0)),
                tuple((multispace0, tag("}"), multispace0)),
            ),
            delimited(
                tuple((tag("="), multispace0)),
                value,
                tuple((multispace0, tag(";"))),
            ),
        )),
        |(a, b, c)| Version::new(a, b, c),
    )(input)
}

fn procedure(input: &str) -> IResult<&str, Procedure> {
    map(
        tuple((
            terminated(
                alt((type_specifier, map(tag("void"), |_| TypeSpecifier::Void))),
                multispace1,
            ),
            identifier,
            delimited(
                tuple((multispace0, tag("("), multispace0)),
                alt((type_specifier, map(tag("void"), |_| TypeSpecifier::Void))),
                tuple((multispace0, tag(")"), multispace0)),
            ),
            delimited(
                tuple((tag("="), multispace0)),
                value,
                tuple((multispace0, tag(";"))),
            ),
        )),
        |(a, b, c, d)| Procedure::new(a, b, c, d),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn assign_ok() {
        let (rest, ret) = assign("a = b").unwrap();
        assert_eq!("", rest);
        assert_eq!(Assign::new("a", Value::identifier("b")), ret);
    }

    #[test]
    fn case_spec_ok() {
        let (rest, ret) = case_spec(
            "case A:
case B:
void;",
        )
        .unwrap();
        assert_eq!("", rest);
        assert_eq!(
            CaseSpec::new(
                vec![Value::identifier("A"), Value::identifier("B")],
                Declaration::void()
            ),
            ret
        );
    }

    #[test]
    fn commnet0_0_ok() {
        let (rest, ret) = comment0("").unwrap();
        assert_eq!("", rest);
        assert_eq!(None, ret);
    }

    #[test]
    fn commnet0_1_ok() {
        let (rest, ret) = comment0("/* a */").unwrap();
        assert_eq!("", rest);
        assert_eq!(Some(vec![" a "]), ret);
    }

    #[test]
    fn commnet0_2_ok() {
        let (rest, ret) = comment0("/* a */ /* b */").unwrap();
        assert_eq!("", rest);
        assert_eq!(Some(vec![" a ", " b "]), ret);
    }

    #[test]
    fn commnet_line_ok() {
        let (rest, ret) = comment_line(
            "%a
",
        )
        .unwrap();
        assert_eq!("", rest);
        assert_eq!("a", ret);
    }

    #[test]
    fn constant_ok_decimal_minus() {
        let (rest, ret) = constant("-1").unwrap();
        assert_eq!("", rest);
        assert_eq!(Constant::Decimal("-1".to_string()), ret);
    }

    #[test]
    fn constant_ok_decimal_plus() {
        let (rest, ret) = constant("1").unwrap();
        assert_eq!("", rest);
        assert_eq!(Constant::Decimal("1".to_string()), ret);
    }

    #[test]
    fn constant_ok_hex() {
        let (rest, ret) = constant("0x0102").unwrap();
        assert_eq!("", rest);
        assert_eq!(Constant::Hex("0102".to_string()), ret);
    }

    #[test]
    fn constant_ok_oct() {
        let (rest, ret) = constant("0102").unwrap();
        assert_eq!("", rest);
        assert_eq!(Constant::Octal("102".to_string()), ret);
    }

    #[test]
    fn constant_def_ok() {
        let (rest, ret) = constant_def("const a = 1;").unwrap();
        assert_eq!("", rest);
        assert_eq!(
            Assign::new("a", Value::constant(Constant::Decimal("1".to_string()))),
            ret
        );
    }

    #[test]
    fn constatn_value_dec_i32() {
        let c = Constant::Decimal("10".to_string());
        let v = c.value::<i32>().unwrap();
        assert_eq!(10i32, v);
    }

    #[test]
    fn constatn_value_hex_i32() {
        let c = Constant::Hex("10".to_string());
        let v = c.value::<i32>().unwrap();
        assert_eq!(16i32, v);
    }

    #[test]
    fn constatn_value_oct_i32() {
        let c = Constant::Octal("10".to_string());
        let v = c.value::<i32>().unwrap();
        assert_eq!(8i32, v);
    }

    #[test]
    fn constatn_value_err_i32() {
        let c = Constant::Decimal("a".to_string());
        let e = c.value::<i32>();
        assert!(e.is_err());
    }

    #[test]
    fn constatn_value_dec_u32() {
        let c = Constant::Decimal("10".to_string());
        let v = c.value::<u32>().unwrap();
        assert_eq!(10u32, v);
    }

    #[test]
    fn constatn_value_hex_u32() {
        let c = Constant::Hex("10".to_string());
        let v = c.value::<u32>().unwrap();
        assert_eq!(16u32, v);
    }

    #[test]
    fn constatn_value_oct_u32() {
        let c = Constant::Octal("10".to_string());
        let v = c.value::<u32>().unwrap();
        assert_eq!(8u32, v);
    }

    #[test]
    fn constatn_value_err_u32() {
        let c = Constant::Decimal("a".to_string());
        let e = c.value::<u32>();
        assert!(e.is_err());
    }

    #[test]
    fn constatn_value_dec_u64() {
        let c = Constant::Decimal("10".to_string());
        let v = c.value::<u64>().unwrap();
        assert_eq!(10u64, v);
    }

    #[test]
    fn constatn_value_hex_u64() {
        let c = Constant::Hex("10".to_string());
        let v = c.value::<u64>().unwrap();
        assert_eq!(16u64, v);
    }

    #[test]
    fn constatn_value_oct_u64() {
        let c = Constant::Octal("10".to_string());
        let v = c.value::<u64>().unwrap();
        assert_eq!(8u64, v);
    }

    #[test]
    fn constatn_value_err_u64() {
        let c = Constant::Decimal("a".to_string());
        let e = c.value::<u64>();
        assert!(e.is_err());
    }

    #[test]
    fn constatn_value_dec_usize() {
        let c = Constant::Decimal("10".to_string());
        let v = c.value::<usize>().unwrap();
        assert_eq!(10usize, v);
    }

    #[test]
    fn constatn_value_hex_usize() {
        let c = Constant::Hex("10".to_string());
        let v = c.value::<usize>().unwrap();
        assert_eq!(16usize, v);
    }

    #[test]
    fn constatn_value_oxt_usize() {
        let c = Constant::Octal("10".to_string());
        let v = c.value::<usize>().unwrap();
        assert_eq!(8usize, v);
    }

    #[test]
    fn constatn_value_err_usize() {
        let c = Constant::Decimal("a".to_string());
        let e = c.value::<usize>();
        assert!(e.is_err());
    }

    #[test]
    fn declaration_variable() {
        let (rest, ret) = declaration("bool a").unwrap();
        assert_eq!("", rest);
        assert_eq!(Declaration::variable(TypeSpecifier::Bool, "a"), ret);
    }

    #[test]
    fn declaration_fixed_array() {
        let (rest, ret) = declaration("bool a[2]").unwrap();
        assert_eq!("", rest);
        assert_eq!(
            Declaration::fixed_array(
                TypeSpecifier::Bool,
                "a",
                Value::constant(Constant::Decimal("2".to_string()))
            ),
            ret
        );
    }

    #[test]
    fn declaration_variable_array() {
        let (rest, ret) = declaration("bool a<2>").unwrap();
        assert_eq!("", rest);
        assert_eq!(
            Declaration::variable_array(
                TypeSpecifier::Bool,
                "a",
                Some(Value::constant(Constant::Decimal("2".to_string())))
            ),
            ret
        );
    }

    #[test]
    fn declaration_opaque_fixed_array() {
        let (rest, ret) = declaration("opaque a[2]").unwrap();
        assert_eq!("", rest);
        assert_eq!(
            Declaration::opaque_fixed_array(
                "a",
                Value::constant(Constant::Decimal("2".to_string()))
            ),
            ret
        );
    }

    #[test]
    fn declaration_opaque_variable_array() {
        let (rest, ret) = declaration("opaque a<2>").unwrap();
        assert_eq!("", rest);
        assert_eq!(
            Declaration::opaque_variable_array(
                "a",
                Some(Value::constant(Constant::Decimal("2".to_string())))
            ),
            ret
        );
    }

    #[test]
    fn declaration_string() {
        let (rest, ret) = declaration("string a<2>").unwrap();
        assert_eq!("", rest);
        assert_eq!(
            Declaration::string(
                "a",
                Some(Value::constant(Constant::Decimal("2".to_string())))
            ),
            ret
        );
    }

    #[test]
    fn declaration_option_variable() {
        let (rest, ret) = declaration("bool *a").unwrap();
        assert_eq!("", rest);
        assert_eq!(Declaration::option_variable(TypeSpecifier::Bool, "a"), ret);
    }

    #[test]
    fn declaration_void() {
        let (rest, ret) = declaration("void").unwrap();
        assert_eq!("", rest);
        assert_eq!(Declaration::void(), ret);
    }

    #[test]
    fn declaration_name_fixed_array() {
        let decl = Declaration::fixed_array(TypeSpecifier::Int(false), "a", Value::identifier("b"));
        let name = decl.name().unwrap();
        assert_eq!("a", name);
    }

    #[test]
    fn declaration_name_opaque_fixed_array() {
        let decl = Declaration::opaque_fixed_array("a", Value::identifier("b"));
        let name = decl.name().unwrap();
        assert_eq!("a", name);
    }

    #[test]
    fn declaration_name_opaque_variable_array() {
        let decl = Declaration::opaque_variable_array("a", None);
        let name = decl.name().unwrap();
        assert_eq!("a", name);
    }

    #[test]
    fn declaration_name_option_variable() {
        let decl = Declaration::option_variable(TypeSpecifier::Int(false), "a");
        let name = decl.name().unwrap();
        assert_eq!("a", name);
    }

    #[test]
    fn declaration_name_string() {
        let decl = Declaration::string("a", None);
        let name = decl.name().unwrap();
        assert_eq!("a", name);
    }

    #[test]
    fn declaration_name_variable() {
        let decl = Declaration::variable(TypeSpecifier::Int(false), "a");
        let name = decl.name().unwrap();
        assert_eq!("a", name);
    }

    #[test]
    fn declaration_name_variable_array() {
        let decl = Declaration::variable_array(TypeSpecifier::Int(false), "a", None);
        let name = decl.name().unwrap();
        assert_eq!("a", name);
    }

    #[test]
    fn declaration_name_void() {
        let decl = Declaration::void();
        let name = decl.name();
        assert!(name.is_none());
    }

    #[test]
    fn declaration_type_specifier_fixed_array() {
        let decl = Declaration::fixed_array(TypeSpecifier::Int(false), "a", Value::identifier("b"));
        let ty = decl.type_specifier().unwrap();
        assert_eq!(TypeSpecifier::Int(false), ty);
    }

    #[test]
    fn declaration_type_specifier_opaque_fixed_array() {
        let decl = Declaration::opaque_fixed_array("a", Value::identifier("b"));
        let ty = decl.type_specifier().unwrap();
        assert_eq!(TypeSpecifier::identifier_type("u8"), ty);
    }

    #[test]
    fn declaration_type_specifier_opaque_variable_array() {
        let decl = Declaration::opaque_variable_array("a", None);
        let ty = decl.type_specifier().unwrap();
        assert_eq!(TypeSpecifier::identifier_type("u8"), ty);
    }

    #[test]
    fn declaration_type_specifier_option_variable() {
        let decl = Declaration::option_variable(TypeSpecifier::Int(false), "a");
        let ty = decl.type_specifier().unwrap();
        assert_eq!(TypeSpecifier::Int(false), ty);
    }

    #[test]
    fn declaration_type_specifier_string() {
        let decl = Declaration::string("a", None);
        let ty = decl.type_specifier().unwrap();
        assert_eq!(TypeSpecifier::identifier_type("String"), ty);
    }

    #[test]
    fn declaration_type_specifier_variable() {
        let decl = Declaration::variable(TypeSpecifier::Int(false), "a");
        let ty = decl.type_specifier().unwrap();
        assert_eq!(TypeSpecifier::Int(false), ty);
    }

    #[test]
    fn declaration_type_specifier_variable_array() {
        let decl = Declaration::variable_array(TypeSpecifier::Int(false), "a", None);
        let ty = decl.type_specifier().unwrap();
        assert_eq!(TypeSpecifier::Int(false), ty);
    }

    #[test]
    fn declaration_type_specifier_void() {
        let decl = Declaration::void();
        let ty = decl.type_specifier();
        assert!(ty.is_none());
    }

    #[test]
    fn definition_constant() {
        let (rest, ret) = definition("const a = 1;").unwrap();
        assert_eq!("", rest);
        assert_eq!(
            Definition::constant_def(Assign::new(
                "a",
                Value::constant(Constant::Decimal("1".to_string()))
            )),
            ret
        );
    }

    #[test]
    fn definition_typdef() {
        let (rest, ret) = definition("typedef void;").unwrap();
        assert_eq!("", rest);
        assert_eq!(
            Definition::type_def(TypeDef::declaration_def(Declaration::void())),
            ret
        );
    }

    #[test]
    fn enum_type_spec_1_ok() {
        let (rest, ret) = enum_type_spec("enum { a = 1 }").unwrap();
        assert_eq!("", rest);
        assert_eq!(
            EnumTypeSpec::new(vec![Assign::new(
                "a",
                Value::constant(Constant::Decimal("1".to_string()))
            ),]),
            ret
        );
    }

    #[test]
    fn enum_type_spec_2_ok() {
        let (rest, ret) = enum_type_spec("enum { a = 1, b = 2}").unwrap();
        assert_eq!("", rest);
        assert_eq!(
            EnumTypeSpec::new(vec![
                Assign::new("a", Value::constant(Constant::Decimal("1".to_string()))),
                Assign::new("b", Value::constant(Constant::Decimal("2".to_string())))
            ]),
            ret
        );
    }

    #[test]
    fn identifier_err() {
        let err = identifier("1a").err().is_some();
        assert_eq!(true, err);
    }

    #[test]
    fn identifier_ok() {
        let (rest, ret) = identifier("a1_1").unwrap();
        assert_eq!("", rest);
        assert_eq!("a1_1", ret);
    }

    #[test]
    fn struct_type_spec_ok() {
        let (rest, ret) = struct_type_spec("struct { void; }").unwrap();
        assert_eq!("", rest);
        assert_eq!(StructTypeSpec::new(vec![Declaration::void()]), ret);
    }

    #[test]
    fn type_def_declaration() {
        let (rest, ret) = type_def("typedef void;").unwrap();
        assert_eq!("", rest);
        assert_eq!(TypeDef::declaration_def(Declaration::void()), ret);
    }

    #[test]
    fn type_def_enum() {
        let (rest, ret) = type_def("enum a { b = 0  };").unwrap();
        assert_eq!("", rest);
        assert_eq!(
            TypeDef::enum_def(
                "a",
                vec![Assign::new(
                    "b",
                    Value::constant(Constant::Decimal("0".to_string()))
                )]
            ),
            ret
        );
    }

    #[test]
    fn type_def_struct() {
        let (rest, ret) = type_def("struct a { void; };").unwrap();
        assert_eq!("", rest);
        assert_eq!(TypeDef::struct_def("a", vec![Declaration::void()]), ret);
    }

    #[test]
    fn type_def_union() {
        let (rest, ret) = type_def("union a switch (void) { case b : void; };").unwrap();
        assert_eq!("", rest);
        assert_eq!(
            TypeDef::union_def(
                "a",
                UnionBody::new(
                    Declaration::void(),
                    vec![CaseSpec::new(
                        vec![Value::identifier("b")],
                        Declaration::void()
                    )],
                    None
                )
            ),
            ret
        );
    }

    #[test]
    fn type_specifier_unsigned_int() {
        let (rest, ret) = type_specifier("unsigned int ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Int(false), ret);
    }

    #[test]
    fn type_specifier_unsigned_inta() {
        let (rest, ret) = type_specifier("unsigned inta ").unwrap();
        assert_eq!(" inta ", rest);
        assert_eq!(TypeSpecifier::Int(false), ret);
    }

    #[test]
    fn type_specifier_int() {
        let (rest, ret) = type_specifier("int ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Int(true), ret);
    }

    #[test]
    fn type_specifier_inta() {
        let (rest, ret) = type_specifier("inta ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Identifier("inta".to_string()), ret);
    }

    #[test]
    fn type_specifier_unsigned_hyper() {
        let (rest, ret) = type_specifier("unsigned hyper ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Hyper(false), ret);
    }

    #[test]
    fn type_specifier_unsigned_hypera() {
        let (rest, ret) = type_specifier("unsigned hypera ").unwrap();
        assert_eq!(" hypera ", rest);
        assert_eq!(TypeSpecifier::Int(false), ret);
    }

    #[test]
    fn type_specifier_hyper() {
        let (rest, ret) = type_specifier("hyper ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Hyper(true), ret);
    }

    #[test]
    fn type_specifier_hypera() {
        let (rest, ret) = type_specifier("hypera ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Identifier("hypera".to_string()), ret);
    }

    #[test]
    fn type_specifier_unsigned_char() {
        let (rest, ret) = type_specifier("unsigned char ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Char(false), ret);
    }

    #[test]
    fn type_specifier_unsigned_chara() {
        let (rest, ret) = type_specifier("unsigned chara ").unwrap();
        assert_eq!(" chara ", rest);
        assert_eq!(TypeSpecifier::Int(false), ret);
    }

    #[test]
    fn type_specifier_char() {
        let (rest, ret) = type_specifier("char ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Char(true), ret);
    }

    #[test]
    fn type_specifier_chara() {
        let (rest, ret) = type_specifier("chara ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Identifier("chara".to_string()), ret);
    }

    #[test]
    fn type_specifier_unsigned_short() {
        let (rest, ret) = type_specifier("unsigned short ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Short(false), ret);
    }

    #[test]
    fn type_specifier_unsigned_shorta() {
        let (rest, ret) = type_specifier("unsigned shorta ").unwrap();
        assert_eq!(" shorta ", rest);
        assert_eq!(TypeSpecifier::Int(false), ret);
    }

    #[test]
    fn type_specifier_short() {
        let (rest, ret) = type_specifier("short ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Short(true), ret);
    }

    #[test]
    fn type_specifier_shorta() {
        let (rest, ret) = type_specifier("shorta ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Identifier("shorta".to_string()), ret);
    }

    #[test]
    fn type_specifier_unsigned() {
        let (rest, ret) = type_specifier("unsigned ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Int(false), ret);
    }

    #[test]
    fn type_specifier_unsigneda() {
        let (rest, ret) = type_specifier("unsigneda ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Identifier("unsigneda".to_string()), ret);
    }

    #[test]
    fn type_specifier_float() {
        let (rest, ret) = type_specifier("float ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Float, ret);
    }

    #[test]
    fn type_specifier_floata() {
        let (rest, ret) = type_specifier("floata ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Identifier("floata".to_string()), ret);
    }

    #[test]
    fn type_specifier_double() {
        let (rest, ret) = type_specifier("double ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Double, ret);
    }

    #[test]
    fn type_specifier_doublea() {
        let (rest, ret) = type_specifier("doublea ").unwrap();
        assert_eq!(" ", rest);
        assert_eq!(TypeSpecifier::Identifier("doublea".to_string()), ret);
    }

    #[test]
    fn value_constant_ok() {
        let (rest, ret) = value("0").unwrap();
        assert_eq!("", rest);
        assert_eq!(Value::constant(Constant::Decimal("0".to_string())), ret);
    }

    #[test]
    fn value_identifier_ok() {
        let (rest, ret) = value("a").unwrap();
        assert_eq!("", rest);
        assert_eq!(Value::identifier("a"), ret);
    }

    #[test]
    fn value_constants_ok_u32() {
        let v = Value::Constant(Constant::Decimal("10".to_string()));
        let n = v.constants::<u32>().unwrap().unwrap();
        assert_eq!(10u32, n);
    }

    #[test]
    fn value_constants_err() {
        let v = Value::Constant(Constant::Decimal("a".to_string()));
        let e = v.constants::<u32>();
        assert!(e.is_err());
    }

    #[test]
    fn value_constants_none() {
        let v = Value::Identifier("a".to_string());
        let n = v.constants::<u32>().unwrap();
        assert!(n.is_none());
    }

    #[test]
    fn value_is_true_true() {
        let v = Value::identifier("TRUE");
        assert!(v.is_true());
    }

    #[test]
    fn value_is_true_false() {
        let v = Value::identifier("a");
        assert!(!v.is_true());
    }

    #[test]
    fn value_is_false_true() {
        let v = Value::identifier("FALSE");
        assert!(v.is_false());
    }

    #[test]
    fn value_is_false_false() {
        let v = Value::identifier("a");
        assert!(!v.is_false());
    }

    #[test]
    fn program_ok() {
        let (rest, ret) =
            program("program prog { version v1 { int proc(void) = a; } = b; } = c;").unwrap();
        assert_eq!("", rest);
        let proc = Procedure::new(
            TypeSpecifier::Int(true),
            "proc",
            TypeSpecifier::Void,
            Value::identifier("a"),
        );
        let v1 = Version::new("v1", vec![proc], Value::identifier("b"));
        assert_eq!(Program::new("prog", vec![v1], Value::identifier("c")), ret);
    }

    #[test]
    fn version_ok() {
        let (rest, ret) = version("version v1 { int proc(void) = a; } = b;").unwrap();
        assert_eq!("", rest);
        let proc = Procedure::new(
            TypeSpecifier::Int(true),
            "proc",
            TypeSpecifier::Void,
            Value::identifier("a"),
        );
        assert_eq!(Version::new("v1", vec![proc], Value::identifier("b")), ret);
    }

    #[test]
    fn procesure_ok() {
        let (rest, ret) = procedure("int proc(void) = a;").unwrap();
        assert_eq!("", rest);
        assert_eq!(
            Procedure::new(
                TypeSpecifier::Int(true),
                "proc",
                TypeSpecifier::Void,
                Value::identifier("a")
            ),
            ret
        );
    }
}

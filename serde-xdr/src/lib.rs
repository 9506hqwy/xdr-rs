pub mod error;
pub mod opaque;
pub mod primitive;

mod de;
mod ser;

pub use de::from_bytes;
pub use ser::to_bytes;

pub trait XdrIndexer {
    type Error;

    fn name_by_index(index: i32) -> Result<&'static str, Self::Error>;
    fn index(&self) -> i32;
}

pub trait XdrUnion<T> {
    type Error;

    fn name_by_value(value: &T) -> Result<&'static str, Self::Error>;
}

#[cfg(test)]
mod de_tests {
    use super::*;
    use serde::Deserialize;
    use std::collections::HashMap;

    #[derive(Debug, Deserialize, PartialEq)]
    struct UnitStruct;

    #[derive(Debug, Deserialize, PartialEq)]
    enum UnitVariant {
        A,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct NewTypeStruct(u8);

    #[derive(Debug, Deserialize, PartialEq)]
    enum NewTypeVariant {
        A(u8),
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct TupleStruct(u8, u8);

    #[derive(Debug, Deserialize, PartialEq)]
    enum TupleVariant {
        A(u8, u8),
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct Struct {
        a: u8,
        b: u8,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    enum StructVariant {
        A { a: u8, b: u8 },
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct ArrayStruct {
        a: [u8; 2],
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct FixedOpaqueStruct {
        #[serde(with = "opaque::fixed")]
        a: [u8; 2],
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct VariableOpaqueStruct {
        #[serde(with = "opaque::variable")]
        a: Vec<u8>,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct VariableOpaqueArray {
        a: Vec<opaque::VariableArray>,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    enum CustomEnum {
        C = 3,
    }

    impl From<i32> for CustomEnum {
        fn from(v: i32) -> Self {
            match v {
                3 => CustomEnum::C,
                _ => unreachable!(),
            }
        }
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct CustomEnumStruct {
        #[serde(with = "primitive::signed32")]
        a: CustomEnum,
    }

    #[test]
    fn from_bytes_bool() {
        let v = [0x00, 0x00, 0x00, 0x00];
        let ret: bool = from_bytes(&v).unwrap();
        assert!(!ret);

        let v = [0x00, 0x00, 0x00, 0x01];
        let ret: bool = from_bytes(&v).unwrap();
        assert!(ret);
    }

    #[test]
    fn from_bytes_i8() {
        let v = [0x00, 0x00, 0x00, 0x00];
        let ret: i8 = from_bytes(&v).unwrap();
        assert_eq!(0, ret);

        let v = [0xFF, 0xFF, 0xFF, 0x80];
        let ret: i8 = from_bytes(&v).unwrap();
        assert_eq!(i8::MIN, ret);

        let v = [0x00, 0x00, 0x00, 0x7F];
        let ret: i8 = from_bytes(&v).unwrap();
        assert_eq!(i8::MAX, ret);
    }

    #[test]
    fn from_bytes_i16() {
        let v = [0x00, 0x00, 0x00, 0x00];
        let ret: i16 = from_bytes(&v).unwrap();
        assert_eq!(0, ret);

        let v = [0xFF, 0xFF, 0x80, 0x00];
        let ret: i16 = from_bytes(&v).unwrap();
        assert_eq!(i16::MIN, ret);

        let v = [0x00, 0x00, 0x7F, 0xFF];
        let ret: i16 = from_bytes(&v).unwrap();
        assert_eq!(i16::MAX, ret);
    }

    #[test]
    fn from_bytes_i32() {
        let v = [0x00, 0x00, 0x00, 0x00];
        let ret: i32 = from_bytes(&v).unwrap();
        assert_eq!(0, ret);

        let v = [0x80, 0x00, 0x00, 0x00];
        let ret: i32 = from_bytes(&v).unwrap();
        assert_eq!(i32::MIN, ret);

        let v = [0x7F, 0xFF, 0xFF, 0xFF];
        let ret: i32 = from_bytes(&v).unwrap();
        assert_eq!(i32::MAX, ret);
    }

    #[test]
    fn from_bytes_i64() {
        let v = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let ret: i64 = from_bytes(&v).unwrap();
        assert_eq!(0, ret);

        let v = [0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let ret: i64 = from_bytes(&v).unwrap();
        assert_eq!(i64::MIN, ret);

        let v = [0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF];
        let ret: i64 = from_bytes(&v).unwrap();
        assert_eq!(i64::MAX, ret);
    }

    #[test]
    fn from_bytes_u8() {
        let v = [0x00, 0x00, 0x00, 0x00];
        let ret: u8 = from_bytes(&v).unwrap();
        assert_eq!(0, ret);

        let v = [0x00, 0x00, 0x00, 0xFF];
        let ret: u8 = from_bytes(&v).unwrap();
        assert_eq!(u8::MAX, ret);
    }

    #[test]
    fn from_bytes_u16() {
        let v = [0x00, 0x00, 0x00, 0x00];
        let ret: u16 = from_bytes(&v).unwrap();
        assert_eq!(0, ret);

        let v = [0x00, 0x00, 0xFF, 0xFF];
        let ret: u16 = from_bytes(&v).unwrap();
        assert_eq!(u16::MAX, ret);
    }

    #[test]
    fn from_bytes_u32() {
        let v = [0x00, 0x00, 0x00, 0x00];
        let ret: u32 = from_bytes(&v).unwrap();
        assert_eq!(0, ret);

        let v = [0xFF, 0xFF, 0xFF, 0xFF];
        let ret: u32 = from_bytes(&v).unwrap();
        assert_eq!(u32::MAX, ret);
    }

    #[test]
    fn from_bytes_u64() {
        let v = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let ret: u64 = from_bytes(&v).unwrap();
        assert_eq!(0, ret);

        let v = [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF];
        let ret: u64 = from_bytes(&v).unwrap();
        assert_eq!(u64::MAX, ret);
    }

    #[test]
    fn from_bytes_f32() {
        let v = [0x00, 0x00, 0x00, 0x00];
        let ret: f32 = from_bytes(&v).unwrap();
        assert_eq!(0f32, ret);

        let v = [0x41, 0x48, 0x00, 0x00];
        let ret: f32 = from_bytes(&v).unwrap();
        assert_eq!(12.5f32, ret);
    }

    #[test]
    fn from_bytes_f64() {
        let v = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let ret: f64 = from_bytes(&v).unwrap();
        assert_eq!(0f64, ret);

        let v = [0x40, 0x29, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let ret: f64 = from_bytes(&v).unwrap();
        assert_eq!(12.5f64, ret);
    }

    #[test]
    fn from_bytes_char() {
        let v = [0x00, 0x00, 0x00, 0x61];
        let ret: char = from_bytes(&v).unwrap();
        assert_eq!('a', ret);

        let v = [0x00, 0x00, 0x30, 0x42];
        let ret: char = from_bytes(&v).unwrap();
        assert_eq!('あ', ret);
    }

    #[test]
    fn from_bytes_string() {
        let v = [0x00, 0x00, 0x00, 0x01, 0x61, 0x00, 0x00, 0x00];
        let ret: String = from_bytes(&v).unwrap();
        assert_eq!("a".to_string(), ret);

        let v = [0x00, 0x00, 0x00, 0x03, 0xE3, 0x81, 0x82, 0x00];
        let ret: String = from_bytes(&v).unwrap();
        assert_eq!("あ".to_string(), ret);

        let v = [0x00, 0x00, 0x00, 0x04, 0x61, 0x62, 0x63, 0x64];
        let ret: String = from_bytes(&v).unwrap();
        assert_eq!("abcd".to_string(), ret);
    }

    #[test]
    fn from_bytes_option() {
        let v = [0x00, 0x00, 0x00, 0x00];
        let ret: Option<u32> = from_bytes(&v).unwrap();
        assert_eq!(None, ret);

        let v = [0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01];
        let ret: Option<u32> = from_bytes(&v).unwrap();
        assert_eq!(Some(1), ret);
    }

    #[test]
    fn from_bytes_unit() {
        let v = [];
        let ret: () = from_bytes(&v).unwrap();
        assert_eq!((), ret);
    }

    #[test]
    fn from_bytes_unit_struct() {
        let v = [];
        let ret: UnitStruct = from_bytes(&v).unwrap();
        assert_eq!(UnitStruct, ret);
    }

    #[test]
    fn from_bytes_newtype_struct() {
        let v = [0x00, 0x00, 0x00, 0x01];
        let ret: NewTypeStruct = from_bytes(&v).unwrap();
        assert_eq!(NewTypeStruct(1), ret);
    }

    #[test]
    fn from_bytes_seq() {
        let v = [
            0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00,
            0x00, 0x03,
        ];
        let ret: Vec<u32> = from_bytes(&v).unwrap();
        assert_eq!(vec![1u32, 2, 3], ret);
    }

    #[test]
    fn from_bytes_tuple() {
        let v = [
            0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03,
        ];
        let ret: (u32, u32, u32) = from_bytes(&v).unwrap();
        assert_eq!((1u32, 2u32, 3u32), ret);
    }

    #[test]
    fn from_bytes_tuple_struct() {
        let v = [0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02];
        let ret: TupleStruct = from_bytes(&v).unwrap();
        assert_eq!(TupleStruct(1, 2), ret);
    }

    #[test]
    fn from_bytes_map() {
        let v = [
            0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02,
        ];
        let ret: HashMap<u32, u32> = from_bytes(&v).unwrap();

        let mut r = HashMap::new();
        r.insert(1, 2);

        assert_eq!(r, ret);
    }

    #[test]
    fn from_bytes_struct() {
        let v = [0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02];
        let ret: Struct = from_bytes(&v).unwrap();

        let r = Struct { a: 1, b: 2 };

        assert_eq!(r, ret);
    }

    #[test]
    fn from_bytes_enum_unit_variant() {
        let v = [0x00, 0x00, 0x00, 0x00];
        let ret: UnitVariant = from_bytes(&v).unwrap();
        assert_eq!(UnitVariant::A, ret);
    }

    #[test]
    fn from_bytes_enum_newtype_variant() {
        let v = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01];
        let ret: NewTypeVariant = from_bytes(&v).unwrap();
        assert_eq!(NewTypeVariant::A(1), ret);
    }

    #[test]
    fn from_bytes_enum_tuple_variant() {
        let v = [
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02,
        ];
        let ret: TupleVariant = from_bytes(&v).unwrap();
        assert_eq!(TupleVariant::A(1, 2), ret);
    }

    #[test]
    fn from_bytes_enum_struct_variant() {
        let v = [
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02,
        ];
        let ret: StructVariant = from_bytes(&v).unwrap();
        assert_eq!(StructVariant::A { a: 1, b: 2 }, ret);
    }

    #[test]
    fn from_bytes_array_struct() {
        let v = [0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02];
        let ret: ArrayStruct = from_bytes(&v).unwrap();
        assert_eq!(ArrayStruct { a: [1, 2] }, ret);
    }

    #[test]
    fn from_bytes_fixed_opaque_struct() {
        let v = [0x01, 0x02, 0x00, 0x00];
        let ret: FixedOpaqueStruct = from_bytes(&v).unwrap();
        assert_eq!(FixedOpaqueStruct { a: [1, 2] }, ret);
    }

    #[test]
    fn from_bytes_variable_opaque_struct() {
        let v = [0x00, 0x00, 0x00, 0x02, 0x01, 0x02, 0x00, 0x00];
        let ret: VariableOpaqueStruct = from_bytes(&v).unwrap();
        assert_eq!(VariableOpaqueStruct { a: vec![1, 2] }, ret);
    }

    #[test]
    fn from_bytes_variable_opaque_array() {
        let v = [
            0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x02, 0x01, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x01, 0x02, 0x03, 0x00,
        ];
        let ret: VariableOpaqueArray = from_bytes(&v).unwrap();
        assert_eq!(
            VariableOpaqueArray {
                a: vec![
                    opaque::VariableArray { data: vec![0x00] },
                    opaque::VariableArray {
                        data: vec![0x01, 0x02]
                    },
                    opaque::VariableArray {
                        data: vec![0x01, 0x02, 0x03]
                    },
                ]
            },
            ret
        );
    }

    #[test]
    fn from_bytes_custom_enum_struct() {
        let v = [0x00, 0x00, 0x00, 0x03];
        let ret: CustomEnumStruct = from_bytes(&v).unwrap();
        assert_eq!(CustomEnumStruct { a: CustomEnum::C }, ret);
    }
}

#[cfg(test)]
mod ser_tests {
    use super::*;
    use serde::Serialize;
    use std::collections::HashMap;
    use std::ffi::CString;

    #[derive(Serialize)]
    struct UnitStruct;

    #[derive(Serialize)]
    enum UnitVariant {
        A,
    }

    #[derive(Serialize)]
    struct NewTypeStruct(u8);

    #[derive(Serialize)]
    enum NewTypeVariant {
        A(u8),
    }

    #[derive(Serialize)]
    struct TupleStruct(u8, u8);

    #[derive(Serialize)]
    enum TupleVariant {
        A(u8, u8),
    }

    #[derive(Serialize)]
    struct Struct {
        a: u8,
        b: u8,
    }

    #[derive(Serialize)]
    enum StructVariant {
        A { a: u8, b: u8 },
    }

    #[derive(Serialize)]
    struct ArrayStruct {
        a: [u8; 2],
    }

    #[derive(Serialize)]
    struct FixedOpaqueStruct {
        #[serde(with = "opaque::fixed")]
        a: [u8; 2],
    }

    #[derive(Serialize)]
    struct VariableOpaqueStruct {
        #[serde(with = "opaque::variable")]
        a: Vec<u8>,
    }

    #[derive(Serialize)]
    struct VariableOpaqueArray {
        a: Vec<opaque::VariableArray>,
    }

    #[derive(Clone, Serialize)]
    enum CustomEnum {
        C = 3,
    }

    impl AsRef<i32> for CustomEnum {
        fn as_ref(&self) -> &'static i32 {
            match self {
                CustomEnum::C => &3,
            }
        }
    }

    #[derive(Serialize)]
    struct CustomEnumStruct {
        #[serde(with = "primitive::signed32")]
        a: CustomEnum,
    }

    #[test]
    fn to_bytes_bool() {
        let ret = to_bytes(&true).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x01], ret);

        let ret = to_bytes(&false).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00], ret);
    }

    #[test]
    fn to_bytes_i8() {
        let ret = to_bytes(&0i8).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00], ret);

        let ret = to_bytes(&i8::MIN).unwrap();
        assert_eq!(vec![0xFF, 0xFF, 0xFF, 0x80], ret);

        let ret = to_bytes(&i8::MAX).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x7F], ret);
    }

    #[test]
    fn to_bytes_i16() {
        let ret = to_bytes(&0i16).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00], ret);

        let ret = to_bytes(&i16::MIN).unwrap();
        assert_eq!(vec![0xFF, 0xFF, 0x80, 0x00], ret);

        let ret = to_bytes(&i16::MAX).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x7F, 0xFF], ret);
    }

    #[test]
    fn to_bytes_i32() {
        let ret = to_bytes(&0i32).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00], ret);

        let ret = to_bytes(&i32::MIN).unwrap();
        assert_eq!(vec![0x80, 0x00, 0x00, 0x00], ret);

        let ret = to_bytes(&i32::MAX).unwrap();
        assert_eq!(vec![0x7F, 0xFF, 0xFF, 0xFF], ret);
    }

    #[test]
    fn to_bytes_i64() {
        let ret = to_bytes(&0i64).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], ret);

        let ret = to_bytes(&i64::MIN).unwrap();
        assert_eq!(vec![0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], ret);

        let ret = to_bytes(&i64::MAX).unwrap();
        assert_eq!(vec![0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF], ret);
    }

    #[test]
    fn to_bytes_u8() {
        let ret = to_bytes(&0u8).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00], ret);

        let ret = to_bytes(&u8::MAX).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0xFF], ret);
    }

    #[test]
    fn to_bytes_u16() {
        let ret = to_bytes(&0u16).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00], ret);

        let ret = to_bytes(&u16::MAX).unwrap();
        assert_eq!(vec![0x00, 0x00, 0xFF, 0xFF], ret);
    }

    #[test]
    fn to_bytes_u32() {
        let ret = to_bytes(&0u32).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00], ret);

        let ret = to_bytes(&u32::MAX).unwrap();
        assert_eq!(vec![0xFF, 0xFF, 0xFF, 0xFF], ret);
    }

    #[test]
    fn to_bytes_u64() {
        let ret = to_bytes(&0i64).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], ret);

        let ret = to_bytes(&u64::MAX).unwrap();
        assert_eq!(vec![0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF], ret);
    }

    #[test]
    fn to_bytes_f32() {
        let ret = to_bytes(&0f32).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00], ret);

        let ret = to_bytes(&12.5f32).unwrap();
        assert_eq!(vec![0x41, 0x48, 0x00, 0x00], ret);
    }

    #[test]
    fn to_bytes_f64() {
        let ret = to_bytes(&0f64).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], ret);

        let ret = to_bytes(&12.5f64).unwrap();
        assert_eq!(vec![0x40, 0x29, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], ret);
    }

    #[test]
    fn to_bytes_char() {
        let ret = to_bytes(&'a').unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x61], ret);

        let ret = to_bytes(&'あ').unwrap();
        assert_eq!(vec![0x00, 0x00, 0x30, 0x42], ret);
    }

    #[test]
    fn to_bytes_str() {
        let ret = to_bytes(&"a").unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x01, 0x61, 0x00, 0x00, 0x00], ret);

        let ret = to_bytes(&"あ").unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x03, 0xE3, 0x81, 0x82, 0x00], ret);

        let ret = to_bytes(&"abcd").unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x04, 0x61, 0x62, 0x63, 0x64], ret);
    }

    #[test]
    fn to_bytes_bytes() {
        let v = CString::new("a").unwrap();
        let ret = to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x01, 0x61, 0x00, 0x00, 0x00], ret);
    }

    #[test]
    fn to_bytes_none() {
        let v: Option<u8> = None;
        let ret = to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00], ret);
    }

    #[test]
    fn to_bytes_some() {
        let v: Option<u8> = Some(2);
        let ret = to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02], ret);
    }

    #[test]
    fn to_bytes_unit() {
        let ret = to_bytes(&()).unwrap();
        assert_eq!(vec![0; 0], ret);
    }

    #[test]
    fn to_bytes_unit_variant() {
        let v = UnitVariant::A;
        let ret = to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00], ret);
    }

    #[test]
    fn to_bytes_new_type_struct() {
        let v = NewTypeStruct(1);
        let ret = to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x01], ret);
    }

    #[test]
    fn to_bytes_new_type_variant() {
        let v = NewTypeVariant::A(1);
        let ret = to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01], ret);
    }

    #[test]
    fn to_bytes_opaque() {
        let v = [0u8; 2];
        let ret = to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], ret);
    }

    #[test]
    fn to_bytes_seq() {
        let v = vec![NewTypeStruct(0), NewTypeStruct(1)];
        let ret = to_bytes(&v).unwrap();
        assert_eq!(
            vec![0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01],
            ret
        );
    }

    #[test]
    fn to_bytes_tuple() {
        let v = (0, 1);
        let ret = to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01], ret);

        let v: [u32; 2] = [1, 2];
        let ret = to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02], ret);
    }

    #[test]
    fn to_bytes_tuple_struct() {
        let v = TupleStruct(0, 1);
        let ret = to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01], ret);
    }

    #[test]
    fn to_bytes_tuple_variant() {
        let v = TupleVariant::A(0, 1);
        let ret = to_bytes(&v).unwrap();
        assert_eq!(
            vec![0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01],
            ret
        );
    }

    #[test]
    fn to_bytes_map() {
        let mut v = HashMap::new();
        v.insert(1, 2);
        let ret = to_bytes(&v).unwrap();
        assert_eq!(
            vec![0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02],
            ret
        );
    }

    #[test]
    fn to_bytes_struct() {
        let v = Struct { a: 1, b: 2 };
        let ret = to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02], ret);
    }

    #[test]
    fn to_bytes_struct_variant() {
        let v = StructVariant::A { a: 1, b: 2 };
        let ret = to_bytes(&v).unwrap();
        assert_eq!(
            vec![0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02],
            ret
        );
    }

    #[test]
    fn to_bytes_array_struct() {
        let v = ArrayStruct { a: [1, 2] };
        let ret = to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02], ret);
    }

    #[test]
    fn to_bytes_fixed_opaque_struct() {
        let v = FixedOpaqueStruct { a: [1, 2] };
        let ret = to_bytes(&v).unwrap();
        assert_eq!(vec![0x01, 0x02, 0x00, 0x00], ret);
    }

    #[test]
    fn to_bytes_variable_opaque_struct() {
        let v = VariableOpaqueStruct { a: vec![1, 2] };
        let ret = to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x02, 0x01, 0x02, 0x00, 0x00], ret);
    }

    #[test]
    fn from_bytes_variable_opaque_array() {
        let v = VariableOpaqueArray {
            a: vec![
                opaque::VariableArray { data: vec![0x00] },
                opaque::VariableArray {
                    data: vec![0x01, 0x02],
                },
                opaque::VariableArray {
                    data: vec![0x01, 0x02, 0x03],
                },
            ],
        };
        let ret = to_bytes(&v).unwrap();
        assert_eq!(
            vec![
                0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x02, 0x01, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x01, 0x02, 0x03, 0x00
            ],
            ret
        );
    }

    #[test]
    fn from_bytes_custom_enum_struct() {
        let v = CustomEnumStruct { a: CustomEnum::C };
        let ret = to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x03], ret);
    }
}

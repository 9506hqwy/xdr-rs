#[cfg(test)]
mod tests {
    use serde::ser::SerializeTuple;
    use serde::{Deserialize, Serialize};
    use serde_xdr;
    use serde_xdr::{XdrIndexer, XdrUnion};
    use serde_xdr_derive::{XdrIndexer, XdrUnion};

    #[derive(Clone, Deserialize, Debug, Serialize, PartialEq)]
    enum Type {
        _Reserved0,
        W = 1,
        _Reserved2,
        X = 3,
        _Reserved4,
        Y = 5,
        _Reserved6,
        Z = 7,
    }

    #[derive(Clone, Deserialize, Debug, Serialize, PartialEq)]
    struct Struct {
        f0: u8,
    }

    #[derive(Clone, Debug, XdrIndexer, PartialEq)]
    enum Indexer {
        A,
        B(u8),
        C(i32, u32),
        D(Struct),
    }

    #[derive(Clone, Debug, XdrUnion, PartialEq)]
    enum UnionI32 {
        A(i32),
        B(i32, u8),
        C(i32, (i32, u32)),
        D(i32, Struct),
    }

    #[derive(Clone, Debug, XdrUnion, PartialEq)]
    enum UnionU32 {
        A(u32),
        B(u32, u8),
        C(u32, (i32, u32)),
        D(u32, Struct),
    }

    #[derive(Clone, Debug, XdrUnion, PartialEq)]
    enum UnionEnum {
        A(Type),
        B(Type, u8),
        C(Type, (i32, u32)),
        D(Type, Struct),
    }

    impl serde_xdr::XdrIndexer for Indexer {
        type Error = serde_xdr::error::Error;

        fn name_by_index(index: i32) -> Result<&'static str, Self::Error> {
            match index {
                1 => Ok("A"),
                3 => Ok("B"),
                5 => Ok("C"),
                7 => Ok("D"),
                _ => unreachable!(),
            }
        }

        fn index(&self) -> i32 {
            match self {
                Indexer::A => 1,
                Indexer::B(_) => 3,
                Indexer::C(_, _) => 5,
                Indexer::D(_) => 7,
            }
        }
    }

    impl serde_xdr::XdrUnion<i32> for UnionI32 {
        type Error = serde_xdr::error::Error;

        fn name_by_value(value: &i32) -> Result<&'static str, Self::Error> {
            match value {
                1 => Ok("A"),
                3 => Ok("B"),
                5 => Ok("C"),
                7 => Ok("D"),
                _ => unreachable!(),
            }
        }
    }

    impl serde_xdr::XdrUnion<u32> for UnionU32 {
        type Error = serde_xdr::error::Error;

        fn name_by_value(value: &u32) -> Result<&'static str, Self::Error> {
            match value {
                1 => Ok("A"),
                3 => Ok("B"),
                5 => Ok("C"),
                7 => Ok("D"),
                _ => unreachable!(),
            }
        }
    }

    impl serde_xdr::XdrUnion<Type> for UnionEnum {
        type Error = serde_xdr::error::Error;

        fn name_by_value(value: &Type) -> Result<&'static str, Self::Error> {
            match value {
                Type::W => Ok("A"),
                Type::X => Ok("B"),
                Type::Y => Ok("C"),
                Type::Z => Ok("D"),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn xdr_indexer_unit_variant() {
        let v = Indexer::A;
        let ret = serde_xdr::to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x01], ret);

        let s = serde_xdr::from_bytes(&ret).unwrap();
        assert_eq!(v, s);
    }

    #[test]
    fn xdr_indexer_newtype_variant() {
        let v = Indexer::B(11);
        let ret = serde_xdr::to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x0B], ret);

        let s = serde_xdr::from_bytes(&ret).unwrap();
        assert_eq!(v, s);
    }

    #[test]
    fn xdr_indexer_tuple_variant() {
        let v = Indexer::C(-1, 2);
        let ret = serde_xdr::to_bytes(&v).unwrap();
        assert_eq!(
            vec![0x00, 0x00, 0x00, 0x05, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x02],
            ret
        );

        let s = serde_xdr::from_bytes(&ret).unwrap();
        assert_eq!(v, s);
    }

    #[test]
    fn xdr_indexer_struct_variant() {
        let v = Indexer::D(Struct { f0: 1 });
        let ret = serde_xdr::to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x01], ret);

        let s = serde_xdr::from_bytes(&ret).unwrap();
        assert_eq!(v, s);
    }

    #[test]
    fn xdr_union_i32_unit_variant() {
        let v = UnionI32::A(1);
        let ret = serde_xdr::to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x01], ret);

        let s = serde_xdr::from_bytes(&ret).unwrap();
        assert_eq!(v, s);
    }

    #[test]
    fn xdr_union_i32_newtype_variant() {
        let v = UnionI32::B(3, 11);
        let ret = serde_xdr::to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x0B], ret);

        let s = serde_xdr::from_bytes(&ret).unwrap();
        assert_eq!(v, s);
    }

    #[test]
    fn xdr_union_i32_tuple_variant() {
        let v = UnionI32::C(5, (-1, 2));
        let ret = serde_xdr::to_bytes(&v).unwrap();
        assert_eq!(
            vec![0x00, 0x00, 0x00, 0x05, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x02],
            ret
        );

        let s = serde_xdr::from_bytes(&ret).unwrap();
        assert_eq!(v, s);
    }

    #[test]
    fn xdr_union_i32_struct_variant() {
        let v = UnionI32::D(7, Struct { f0: 1 });
        let ret = serde_xdr::to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x01], ret);

        let s = serde_xdr::from_bytes(&ret).unwrap();
        assert_eq!(v, s);
    }

    #[test]
    fn xdr_union_u32_unit_variant() {
        let v = UnionU32::A(1);
        let ret = serde_xdr::to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x01], ret);

        let s = serde_xdr::from_bytes(&ret).unwrap();
        assert_eq!(v, s);
    }

    #[test]
    fn xdr_union_u32_newtype_variant() {
        let v = UnionU32::B(3, 11);
        let ret = serde_xdr::to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x0B], ret);

        let s = serde_xdr::from_bytes(&ret).unwrap();
        assert_eq!(v, s);
    }

    #[test]
    fn xdr_union_u32_tuple_variant() {
        let v = UnionU32::C(5, (-1, 2));
        let ret = serde_xdr::to_bytes(&v).unwrap();
        assert_eq!(
            vec![0x00, 0x00, 0x00, 0x05, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x02],
            ret
        );

        let s = serde_xdr::from_bytes(&ret).unwrap();
        assert_eq!(v, s);
    }

    #[test]
    fn xdr_union_u32_struct_variant() {
        let v = UnionU32::D(7, Struct { f0: 1 });
        let ret = serde_xdr::to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x01], ret);

        let s = serde_xdr::from_bytes(&ret).unwrap();
        assert_eq!(v, s);
    }

    #[test]
    fn xdr_union_enum_unit_variant() {
        let v = UnionEnum::A(Type::W);
        let ret = serde_xdr::to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x01], ret);

        let s = serde_xdr::from_bytes(&ret).unwrap();
        assert_eq!(v, s);
    }

    #[test]
    fn xdr_union_enum_newtype_variant() {
        let v = UnionEnum::B(Type::X, 11);
        let ret = serde_xdr::to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x0B], ret);

        let s = serde_xdr::from_bytes(&ret).unwrap();
        assert_eq!(v, s);
    }

    #[test]
    fn xdr_union_enum_tuple_variant() {
        let v = UnionEnum::C(Type::Y, (-1, 2));
        let ret = serde_xdr::to_bytes(&v).unwrap();
        assert_eq!(
            vec![0x00, 0x00, 0x00, 0x05, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x02],
            ret
        );

        let s = serde_xdr::from_bytes(&ret).unwrap();
        assert_eq!(v, s);
    }

    #[test]
    fn xdr_union_enum_struct_variant() {
        let v = UnionEnum::D(Type::Z, Struct { f0: 1 });
        let ret = serde_xdr::to_bytes(&v).unwrap();
        assert_eq!(vec![0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x01], ret);

        let s = serde_xdr::from_bytes(&ret).unwrap();
        assert_eq!(v, s);
    }
}

#[cfg(test)]
mod tests {
    use serde::ser::SerializeTuple;
    use serde::{Deserialize, Serialize};
    use serde_xdr;
    use serde_xdr::XdrIndexer;
    use serde_xdr_derive::XdrIndexer;

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
}

use serde::de::{SeqAccess, Visitor};
use serde::{Deserialize, Serialize};
use std::fmt;
use std::marker::PhantomData;

#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct VariableArray {
    #[serde(with = "variable")]
    pub data: Vec<u8>,
}

struct OpaqueVisitor<T> {
    marker: PhantomData<T>,
}

impl<T> OpaqueVisitor<T> {
    fn new() -> Self {
        OpaqueVisitor {
            marker: PhantomData,
        }
    }
}

impl<'de, const N: usize> Visitor<'de> for OpaqueVisitor<[u8; N]> {
    type Value = [u8; N];

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "a byte array")
    }

    fn visit_seq<V>(self, seq: V) -> Result<Self::Value, V::Error>
    where
        V: SeqAccess<'de>,
    {
        Ok(enumerate_u8(seq)?.try_into().unwrap())
    }
}

impl<'de> Visitor<'de> for OpaqueVisitor<Vec<u8>> {
    type Value = Vec<u8>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "a byte array")
    }

    fn visit_seq<V>(self, seq: V) -> Result<Self::Value, V::Error>
    where
        V: SeqAccess<'de>,
    {
        enumerate_u8(seq)
    }
}

fn enumerate_u32(buf: &mut Vec<u8>) -> Vec<u32> {
    let mut numbers = vec![];

    for bytes in buf.as_mut_slice().chunks_mut(4) {
        let mut number = [0u8; 4];
        bytes.swap_with_slice(&mut number[..bytes.len()]);
        numbers.push(u32::from_be_bytes(number));
    }

    numbers
}

fn enumerate_u8<'de, V>(mut seq: V) -> Result<Vec<u8>, V::Error>
where
    V: SeqAccess<'de>,
{
    let size = seq.size_hint().unwrap();

    let mut bytes = vec![0u8; size];
    let padded_len = size.div_ceil(4);

    let mut idx = 0;
    for _ in 0..padded_len {
        if let Some(v) = seq.next_element::<u32>()? {
            for b in v.to_be_bytes() {
                if idx < size {
                    bytes[idx] = b;
                    idx += 1;
                }
            }
        }
    }

    Ok(bytes)
}

pub mod fixed {
    use serde::de::Deserializer;
    use serde::ser::{Serialize, SerializeTuple, Serializer};

    pub fn serialize<T, S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
    where
        T: Serialize + AsRef<[u8]>,
        S: Serializer,
    {
        let mut buf = value.as_ref().to_vec();

        let mut ser = serializer.serialize_tuple(buf.len())?;

        for number in super::enumerate_u32(&mut buf) {
            ser.serialize_element(&number)?;
        }

        ser.end()
    }

    pub fn deserialize<'de, const N: usize, D>(deserializer: D) -> Result<[u8; N], D::Error>
    where
        D: Deserializer<'de>,
    {
        let visitor = super::OpaqueVisitor::<[u8; N]>::new();
        deserializer.deserialize_tuple(N, visitor)
    }
}

pub mod variable {
    use serde::de::Deserializer;
    use serde::ser::{Serialize, SerializeSeq, Serializer};

    pub fn serialize<T, S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
    where
        T: Serialize + AsRef<[u8]>,
        S: Serializer,
    {
        let mut buf = value.as_ref().to_vec();

        let mut ser = serializer.serialize_seq(Some(buf.len()))?;

        for number in super::enumerate_u32(&mut buf) {
            ser.serialize_element(&number)?;
        }

        ser.end()
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Vec<u8>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let visitor = super::OpaqueVisitor::<Vec<u8>>::new();
        deserializer.deserialize_seq(visitor)
    }
}

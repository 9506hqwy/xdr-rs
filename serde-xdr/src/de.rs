use crate::error::Error;
use serde::de::IntoDeserializer;
use serde::{de, de::DeserializeOwned};
use std::convert::TryInto;
use std::str;

struct Deserializer<'de> {
    input: &'de [u8],
}

struct Seq<'a, 'de: 'a> {
    count: usize,
    de: &'a mut Deserializer<'de>,
}

struct Map<'a, 'de: 'a> {
    count: usize,
    de: &'a mut Deserializer<'de>,
}

struct Enum<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
}

pub fn from_bytes<T>(value: &[u8]) -> Result<T, Error>
where
    T: DeserializeOwned,
{
    let mut deserializer = Deserializer { input: value };
    let t = T::deserialize(&mut deserializer)?;
    if deserializer.input.is_empty() {
        Ok(t)
    } else {
        Err(Error::TrailingData(deserializer.input.len()))
    }
}

impl<'de> Deserializer<'de> {
    fn read(&mut self, len: usize) -> Result<&'de [u8], Error> {
        if self.input.len() < len {
            return Err(Error::NotEnoughData(len - self.input.len()));
        }

        let (v, rest) = self.input.split_at(len);
        self.input = rest;
        Ok(v)
    }

    fn read_i32(&mut self) -> Result<i32, Error> {
        let v = self.read(4)?;
        Ok(i32::from_be_bytes(v.try_into()?))
    }

    fn read_i64(&mut self) -> Result<i64, Error> {
        let v = self.read(8)?;
        Ok(i64::from_be_bytes(v.try_into()?))
    }

    fn read_u32(&mut self) -> Result<u32, Error> {
        let v = self.read(4)?;
        Ok(u32::from_be_bytes(v.try_into()?))
    }

    fn read_u64(&mut self) -> Result<u64, Error> {
        let v = self.read(8)?;
        Ok(u64::from_be_bytes(v.try_into()?))
    }

    fn read_f32(&mut self) -> Result<f32, Error> {
        let v = self.read(4)?;
        Ok(f32::from_be_bytes(v.try_into()?))
    }

    fn read_f64(&mut self) -> Result<f64, Error> {
        let v = self.read(8)?;
        Ok(f64::from_be_bytes(v.try_into()?))
    }

    fn read_char(&mut self) -> Result<char, Error> {
        let code = self.read_u32()?;
        char::from_u32(code).ok_or(Error::Convert)
    }

    fn read_string(&mut self) -> Result<&'de str, Error> {
        let size = self.read_u32()? as usize;
        let padding_size = 4 - size % 4;

        let v = self.read(size)?;
        let v = str::from_utf8(v)?;
        if padding_size < 4 {
            self.input = &self.input[padding_size..];
        }
        Ok(v)
    }

    fn read_bytes(&mut self) -> Result<&'de [u8], Error> {
        let size = self.read_u32()? as usize;
        let padding_size = 4 - size % 4;

        let v = self.read(size)?;
        if padding_size < 4 {
            self.input = &self.input[padding_size..];
        }
        Ok(v)
    }
}

impl<'de, 'b> de::Deserializer<'de> for &'b mut Deserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Err(Error::NotImplemented)
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_bool(self.read_u32()? != 0)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i8(self.read_i32()? as i8)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i16(self.read_i32()? as i16)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i32(self.read_i32()?)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i64(self.read_i64()?)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u8(self.read_u32()? as u8)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u16(self.read_u32()? as u16)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u32(self.read_u32()?)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u64(self.read_u64()?)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_f32(self.read_f32()?)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_f64(self.read_f64()?)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_char(self.read_char()?)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.read_string()?)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.read_string()?)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_borrowed_bytes(self.read_bytes()?)
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_borrowed_bytes(self.read_bytes()?)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        if self.read_u32()? == 0 {
            visitor.visit_none()
        } else {
            visitor.visit_some(self)
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let count = self.read_u32()? as usize;
        visitor.visit_seq(Seq::new(count, self))
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_seq(Seq::new(len, self))
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_seq(Seq::new(len, self))
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let count = self.read_u32()? as usize;
        visitor.visit_map(Map::new(count, self))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_seq(Seq::new(fields.len(), self))
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_enum(Enum::new(self))
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }
}

impl<'a, 'de> Seq<'a, 'de> {
    fn new(count: usize, de: &'a mut Deserializer<'de>) -> Self {
        Seq { count, de }
    }
}

impl<'a, 'de> de::SeqAccess<'de> for Seq<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        if self.count == 0 {
            return Ok(None);
        }

        self.count -= 1;
        seed.deserialize(&mut *self.de).map(Some)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.count)
    }
}

impl<'a, 'de> Map<'a, 'de> {
    fn new(count: usize, de: &'a mut Deserializer<'de>) -> Self {
        Map { count, de }
    }
}

impl<'a, 'de> de::MapAccess<'de> for Map<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        if self.count == 0 {
            return Ok(None);
        }

        self.count -= 1;
        seed.deserialize(&mut *self.de).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        seed.deserialize(&mut *self.de)
    }
}

impl<'a, 'de> Enum<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Enum { de }
    }
}

impl<'a, 'de> de::EnumAccess<'de> for Enum<'a, 'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let index: usize = self.de.read_i32()?.try_into().map_err(|_| Error::Convert)?;
        let de: de::value::UsizeDeserializer<Self::Error> = index.into_deserializer();
        let v = seed.deserialize(de)?;
        Ok((v, self))
    }
}

impl<'a, 'de> de::VariantAccess<'de> for Enum<'a, 'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(&mut *self.de)
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_tuple(self.de, len, visitor)
    }

    fn struct_variant<V>(
        self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_tuple(self.de, fields.len(), visitor)
    }
}

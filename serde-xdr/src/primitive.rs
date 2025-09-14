pub mod signed32 {
    use serde::de::{Deserialize, Deserializer, Error};
    use serde::ser::Serializer;

    pub fn serialize<T, S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
    where
        T: AsRef<i32>,
        S: Serializer,
    {
        serializer.serialize_i32(*value.as_ref())
    }

    pub fn deserialize<'de, T, D>(deserializer: D) -> Result<T, D::Error>
    where
        T: TryFrom<i32>,
        D: Deserializer<'de>,
    {
        let v = i32::deserialize(deserializer)?;
        T::try_from(v).map_err(|_| Error::custom(format!("Can not convert from {v}")))
    }
}

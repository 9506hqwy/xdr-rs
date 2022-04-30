use serde::{de, ser};
use std::array;
use std::error;
use std::fmt;
use std::num;
use std::str;

#[derive(Debug)]
pub enum Error {
    Custom(String),
    Convert,
    Encoding(str::Utf8Error),
    NotEnoughData(usize),
    NotImplemented,
    NotSupported,
    TrailingData,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "error: {:?}", self)
    }
}

impl error::Error for Error {}

impl de::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        Error::Custom(format!("{}", msg))
    }
}

impl ser::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        Error::Custom(format!("{}", msg))
    }
}

impl From<num::TryFromIntError> for Error {
    fn from(_: num::TryFromIntError) -> Self {
        Error::Convert
    }
}

impl From<array::TryFromSliceError> for Error {
    fn from(_: array::TryFromSliceError) -> Self {
        Error::Convert
    }
}

impl From<str::Utf8Error> for Error {
    fn from(error: str::Utf8Error) -> Self {
        Error::Encoding(error)
    }
}

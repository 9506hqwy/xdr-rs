use nom;
use std::num;

#[derive(Debug)]
pub enum Error {
    NotFountIdentifier(String),
    NotSupported,
    Parse(String),
    TrailingData,
}

impl From<num::ParseIntError> for Error {
    fn from(error: num::ParseIntError) -> Self {
        Error::Parse(format!("{error}"))
    }
}

impl<'a> From<nom::Err<nom::error::Error<&'a str>>> for Error {
    fn from(error: nom::Err<nom::error::Error<&'a str>>) -> Self {
        Error::Parse(format!("{error}"))
    }
}

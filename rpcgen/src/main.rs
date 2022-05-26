use rpcgen::{gen, parser};
use std::env;
use std::fs;
use std::io;

fn main() -> Result<(), Error> {
    let path = env::args().nth(1).ok_or(Error::Arg)?;
    let source = fs::read_to_string(&path)?;

    let config = gen::Config {
        remove_typedef: true,
        complement_enum_index: false,
    };

    let decls = parser::parse(&source)?;
    let bindings = gen::gen(decls, &config)?;

    println!("{}", bindings);

    Ok(())
}

#[derive(Debug)]
enum Error {
    Arg,
    Io(io::Error),
    RpcGen(rpcgen::error::Error),
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Error::Io(error)
    }
}

impl From<rpcgen::error::Error> for Error {
    fn from(error: rpcgen::error::Error) -> Self {
        Error::RpcGen(error)
    }
}

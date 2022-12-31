use clap::{Arg, Command};
use rpcgen::{gen, parser};
use std::fs;
use std::io;

fn main() -> Result<(), Error> {
    let matches = Command::new("Code Generation")
        .version("0.3.0")
        .arg(
            Arg::new("path")
                .value_name("PATH")
                .required(true)
                .help("protocol file."),
        )
        .arg(
            Arg::new("use-std-trait")
                .long("use-std-trait")
                .takes_value(false),
        )
        .arg(
            Arg::new("use-extra-trait")
                .long("use-extra-trait")
                .takes_value(false),
        )
        .arg(
            Arg::new("use-indexer")
                .long("use-indexer")
                .takes_value(false),
        )
        .arg(Arg::new("use-union").long("use-union").takes_value(false))
        .get_matches();

    let path = matches.value_of("path").unwrap();
    let source = fs::read_to_string(&path)?;

    let enum_impl_std_trait = matches.is_present("use-std-trait");
    let enum_impl_indexer = !enum_impl_std_trait && matches.is_present("use-indexer");
    let union_impl_union = matches.is_present("use-union");
    let union_impl_indexer = !union_impl_union
        && (matches.is_present("use-extra-trait") || matches.is_present("use-indexer"));

    let config = gen::Config {
        remove_typedef: true,
        enum_impl_indexer,
        enum_impl_std_trait,
        union_impl_indexer,
        union_impl_union,
    };

    let decls = parser::parse(&source)?;
    let bindings = gen::gen(decls, &config)?;

    println!("{}", bindings);

    Ok(())
}

#[derive(Debug)]
enum Error {
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

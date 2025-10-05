// https://doc.rust-lang.org/book/appendix-01-keywords.html
// https://doc.rust-lang.org/reference/keywords.html
#[rustfmt::skip]
const KEYWORDS_RUST: &[&str] = &[
    // Keywords Currently in Use
    "as",
    "async",
    "await",
    "break",
    "const",
    "continue",
    "crate",
    "dyn",
    "else",
    "enum",
    "extern",
    "false",
    "fn",
    "for",
    "if",
    "impl",
    "in",
    "let",
    "loop",
    "match",
    "mod",
    "move",
    "mut",
    "pub",
    "ref",
    "return",
    "Self",
    "self",
    "static",
    "struct",
    "super",
    "trait",
    "true",
    "type",
    "union",
    "unsafe",
    "use",
    "where",
    "while",
    // Keywords Reserved for Future Use
    "abstract",
    "become",
    "box",
    "do",
    "final",
    "gen",
    "macro",
    "override",
    "priv",
    "try",
    "typeof",
    "unsized",
    "virtual",
    "yield",
];

// https://datatracker.ietf.org/doc/html/rfc4506#section-6.4
const KEYWORDS_XDR: &[&str] = &[
    "bool",
    "case",
    "const",
    "default",
    "double",
    "quadruple",
    "enum",
    "float",
    "hyper",
    "int",
    "opaque",
    "string",
    "struct",
    "switch",
    "typedef",
    "union",
    "unsigned",
    "void",
    // start (rpcl extension)
    "program",
    "version",
    // end (rpcl extension)
];

pub fn rust_reserved(keyword: &str) -> bool {
    KEYWORDS_RUST.contains(&keyword)
}

pub fn xdr_reserved(keyword: &str) -> bool {
    KEYWORDS_XDR.contains(&keyword)
}

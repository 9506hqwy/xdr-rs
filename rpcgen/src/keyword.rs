// https://doc.rust-lang.org/book/appendix-01-keywords.html
const KEYWORDS_RUST: &[&str] = &[
    "as", "async", "await", "break", "const", "continue", "crate", "dyn", "else", "enum", "extern",
    "false", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub",
    "ref", "return", "Self", "self", "static", "struct", "super", "trait", "true", "type", "union",
    "unsafe", "use", "where", "while",
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
];

pub fn rust_reserved(keyword: &str) -> bool {
    KEYWORDS_RUST.iter().any(|&k| k == keyword)
}

pub fn xdr_reserved(keyword: &str) -> bool {
    KEYWORDS_XDR.iter().any(|&k| k == keyword)
}

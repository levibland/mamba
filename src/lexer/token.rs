use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    String(String),
    Number(f64),
    Ident(String),
    Boolean(bool),

    // Keywords
    Let,
    Fn,
    If,
    Else,
    Return,

    // Operators
    Plus,
    Minus,
    Asterisk,
    Slash,
    Assign,
    Eq,
    NotEq,
    Bang,
    Gt,
    Lt,
    GtEq,
    LtEq,

    // Punctuation
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Colon,
    Semicolon,

    // Special tokens
    Eof,
    Illegal(char),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Token::*;

        match self {
            String(s) => write!(f, "\"{}\"", s),
            Number(n) => write!(f, "{}", n),
            Ident(i) => write!(f, "let {};", i),
            Boolean(b) => write!(f, "{}", b),
            Let => write!(f, "let"),
            Fn => write!(f, "fn"),
            If => write!(f, "if"),
            Else => write!(f, "else"),
            Return => write!(f, "return"),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Asterisk => write!(f, "*"),
            Slash => write!(f, "/"),
            Assign => write!(f, "="),
            Eq => write!(f, "=="),
            NotEq => write!(f, "!="),
            Bang => write!(f, "!"),
            Gt => write!(f, ">"),
            Lt => write!(f, "<"),
            GtEq => write!(f, ">="),
            LtEq => write!(f, "<="),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            LBracket => write!(f, "["),
            RBracket => write!(f, "]"),
            Comma => write!(f, ","),
            Colon => write!(f, ":"),
            Semicolon => write!(f, ";"),
            Eof => write!(f, "\0"),
            Illegal(c) => write!(f, "Illegal: {}", c),
        }
    }
}
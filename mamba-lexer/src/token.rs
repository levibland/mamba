use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Delimiter {
    Paren,
    Brack,
    Brace,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Illegal(char),
    String(String),
    Number(i64),
    Ident(String),
    Boolean(bool),
    Open(Delimiter),
    Close(Delimiter),

    Let,
    Fn,
    If,
    Else,
    Return,

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
    
    Comma,
    Colon,
    Semicolon,

    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Token::*;

        match self {
            Illegal(c) => write!(f, "Illegal: {}", c),
            String(s) => write!(f, "\"{}\"", s),
            Number(n) => write!(f, "{}", n),
            Ident(i) => write!(f, "let {};", i),
            Boolean(b) => write!(f, "{}", b),
            Open(Delimiter::Paren) => write!(f, "("),
            Open(Delimiter::Brack) => write!(f, "["),
            Open(Delimiter::Brace) => write!(f, "{{"),
            Close(Delimiter::Paren) => write!(f, ")"),
            Close(Delimiter::Brack) => write!(f, "]"),
            Close(Delimiter::Brace) => write!(f, "}}"),
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
            Comma => write!(f, ","),
            Colon => write!(f, ":"),
            Semicolon => write!(f, ";"),
            Eof => write!(f, "\0"),
        }
    }
}
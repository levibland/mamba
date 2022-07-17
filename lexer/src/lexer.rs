use logos::{Lexer, Logos};

pub struct Lex;

impl Lex {
    pub fn lex(input: &str) -> Vec<Token> {
        Token::lexer(input).collect()
    }
}

#[derive(Debug, Clone, Logos, PartialEq)]
pub enum Token {
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("return")]
    Return,

    #[regex(r"[a-zA-Z_?]+", to_string)]
    Identifier(String),

    #[regex(r"[0-9]+", to_int)]
    Integer(i64),
    #[regex(r##""(?:[^"\\]|\\.)*""##, to_string)]
    String(String),

    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token("|")]
    Pipe,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("**")]
    Exponential,

    #[token("=")]
    Assign,
    #[token("==")]
    Equals,
    #[token("!=")]
    NotEquals,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("<=")]
    LessThanEquals,
    #[token(">=")]
    GreaterThanEquals,

    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("nil")]
    Nil,

    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,
    #[token("!")]
    Bang,
    #[token(".")]
    Dot,

    Eof,

    #[error]
    #[regex(r";;[^\n]*", logos::skip)]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

impl Into<String> for Token {
    fn into(self) -> String {
        match self {
            Token::Identifier(s) => s,
            Token::String(s) => s,
            _ => unreachable!(),
        }
    }
}

fn to_string(lex: &mut Lexer<Token>) -> Option<String> {
    let mut str = lex.slice().to_string();

    if str.starts_with("$") {
        str.remove(0);
    }

    if str.starts_with("\"") {
        str.remove(0);
    }

    if str.ends_with('"') {
        str.remove(str.len() - 1);
    }

    Some(str)
}

fn to_int(lex: &mut Lexer<Token>) -> Option<i64> {
    Some(lex.slice().parse().ok()?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn skip_comments() {
        let mut lexer = Token::lexer(";; test");

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn recognize_keywords() {
        let mut lexer = Token::lexer("fn let true false if else");

        assert_eq!(lexer.next(), Some(Token::Fn));
        assert_eq!(lexer.next(), Some(Token::Let));
        assert_eq!(lexer.next(), Some(Token::True));
        assert_eq!(lexer.next(), Some(Token::False));
        assert_eq!(lexer.next(), Some(Token::If));
        assert_eq!(lexer.next(), Some(Token::Else));
    }

    #[test]
    fn recognize_symbols() {
        let mut lexer = Token::lexer("( ) { } +-*/ = == != : . ;");

        assert_eq!(lexer.next(), Some(Token::LeftParen));
        assert_eq!(lexer.next(), Some(Token::RightParen));
        assert_eq!(lexer.next(), Some(Token::LeftBrace));
        assert_eq!(lexer.next(), Some(Token::RightBrace));
        assert_eq!(lexer.next(), Some(Token::Plus));
        assert_eq!(lexer.next(), Some(Token::Minus));
        assert_eq!(lexer.next(), Some(Token::Asterisk));
        assert_eq!(lexer.next(), Some(Token::Slash));
        assert_eq!(lexer.next(), Some(Token::Assign));
        assert_eq!(lexer.next(), Some(Token::Equals));
        assert_eq!(lexer.next(), Some(Token::NotEquals));
        assert_eq!(lexer.next(), Some(Token::Colon));
        assert_eq!(lexer.next(), Some(Token::Dot));
        assert_eq!(lexer.next(), Some(Token::SemiColon));
    }

    #[test]
    fn recognize_identifiers() {
        let mut lexer = Token::lexer("test test_ Test test?");

        assert_eq!(lexer.next(), Some(Token::Identifier("test".to_string())));
        assert_eq!(lexer.next(), Some(Token::Identifier("test_".to_string())));
        assert_eq!(lexer.next(), Some(Token::Identifier("Test".to_string())));
        assert_eq!(lexer.next(), Some(Token::Identifier("test?".to_string())));
    }

    #[test]
    fn recognize_integers() {
        let mut lexer = Token::lexer("123 1234");

        assert_eq!(lexer.next(), Some(Token::Integer(123)));
        assert_eq!(lexer.next(), Some(Token::Integer(1234)));
    }

    #[test]
    fn recognize_strings() {
        let mut lexer = Token::lexer(r##""test" "test \"" "test \n""##);

        assert_eq!(lexer.next(), Some(Token::String(r##"test"##.to_string())));
        assert_eq!(lexer.next(), Some(Token::String(r##"test \""##.to_string())));
        assert_eq!(lexer.next(), Some(Token::String(r##"test \n"##.to_string())));
    }
}
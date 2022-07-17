use crate::token::*;

/**
 * 
 * Lexer.
 * 
 * {source}: The source code to be lexed.
 * {current}: The index of the current char.
 * {next}: The index of the next char.
 * {ch}: The current char.
 * 
 */
#[derive(Debug)]
pub struct Lexer {
    source: Vec<char>,

    current: usize,
    next: usize,

    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Self {
            source: input.chars().collect(),
            current: 0,
            next: 0,
            ch: '\0',
        };

        l.read();

        l
    }

    fn read(&mut self) {
        self.ch = match self.source.get(self.next) {
            Some(&char) => char,
            None => '\0',
        };

        self.current = self.next;
        self.next += 1;
    }

    fn peek(&self) -> char {
        match self.source.get(self.next) {
            Some(&char) => char,
            None => '\0',
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            '+' => { self.read(); Token::Plus },
            '-' => { self.read(); Token::Minus },
            '*' => { self.read(); Token::Asterisk },
            '/' => { self.read(); Token::Slash },
            '=' => {
                if self.peek() == '=' {
                    self.read();
                    self.read();
                    Token::Eq
                } else if self.peek() == '>' {
                    self.read();
                    self.read();
                    Token::FatArrow
                } else {
                    self.read();
                    Token::Assign
                }
            },
            '!' => {
                if self.peek() == '=' {
                    self.read();
                    Token::NotEq
                } else {
                    self.read();
                    Token::Bang
                }
            },
            '>' => {
                if self.peek() == '=' {
                    self.read();
                    Token::GtEq
                } else {
                    self.read();
                    Token::Gt
                }
            },
            '<' => {
                if self.peek() == '=' {
                    self.read();
                    Token::LtEq
                } else {
                    self.read();
                    Token::Lt
                }
            },
            '(' => { self.read(); Token::Open(Delimiter::Paren) },
            ')' => { self.read(); Token::Close(Delimiter::Paren) },
            '{' => { self.read(); Token::Open(Delimiter::Brace) },
            '}' => { self.read(); Token::Close(Delimiter::Brace) },
            '[' => { self.read(); Token::Open(Delimiter::Brack) },
            ']' => { self.read(); Token::Close(Delimiter::Brack) },
            ',' => { self.read(); Token::Comma },
            ':' => { self.read(); Token::Colon },
            ';' => { self.read(); Token::Semicolon },
            '|' => { self.read(); Token::Pipe },
            '\0' => { self.read(); Token::Eof },
            c if c.is_letter() => {
                let str = self.read_ident();

                self.lookup_ident(str)
            },
            c if c.is_numeric() => {
                let num = self.read_number();

                Token::Number(num)
            },
            c if c == '"' => {
                let str = self.read_string();

                Token::String(str)
            },
            c => Token::Illegal(c),
        };

        tok
    }

    fn lookup_ident(&self, s: String) -> Token {
        match s.as_str() {
            "fn" => Token::Fn,
            "let" => Token::Let,
            "true" => Token::Boolean(true),
            "false" => Token::Boolean(false),
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Ident(s),
        }
    }

    fn read_ident(&mut self) -> String {
        let start = self.current;
        while self.ch.is_letter() {
            self.read();
        }

        self.source[start..self.current].iter().collect()
    }

    fn read_number(&mut self) -> i64 {
        let start = self.current;
        while self.ch.is_numeric() {
            if self.peek() == '.' {
                self.read();
            }
            self.read();
        }

        let s: String = self.source[start..self.current].iter().collect();

        s.parse().expect("Invalid Number")
    }

    fn read_string(&mut self) -> String {
        let start = self.current + 1;
        loop {
            self.read();
            if self.ch == '"' || self.ch == '\0' {
                break;
            }
        }

        self.source[start..self.current].iter().collect()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read();
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current <= self.source.len() {
            Some(self.next_token())
        } else {
            None
        }
    }
}

trait IsLetter {
    fn is_letter(&self) -> bool;
}

impl IsLetter for char {
    fn is_letter(&self) -> bool {
        self.is_alphabetic() || *self == '_'
    }
}

// TODO: Finish unit tests
#[cfg(test)]
mod lexer_test {
    use super::*;

    fn produces_tokens(input: &str, tokens: Vec<Token>) {
        let lexer = Lexer::new(input.to_string());

        for (tok, expected) in lexer.zip(tokens.iter()) {
            println!("{}, {}", expected, &tok);
            assert_eq!(*expected, tok);
        }
    }

    #[test]
    fn test_next_token() {
        let input = r"let one = 1; let two = 2; fn sub(x, y) { x - y; }";

        let tokens = vec![
            Token::Let,
            Token::Ident("one".to_string()),
            Token::Assign,
            Token::Number(1i64),
            Token::Semicolon,
            Token::Let,
            Token::Ident("two".to_string()),
            Token::Assign,
            Token::Number(2i64),
            Token::Semicolon,
            Token::Fn,
            Token::Ident("sub".to_string()),
            Token::Open(Delimiter::Paren),
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::Close(Delimiter::Paren),
            Token::Open(Delimiter::Brace),
            Token::Ident("x".to_string()),
            Token::Minus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::Close(Delimiter::Brace),
        ];

        produces_tokens(input, tokens);
    }
}
use std::slice::Iter;
use thiserror::Error;
use colored::*;
use lexer::Token;

use crate::ast::*;

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected token {0:?}.")]
    UnexpectedToken(Token),
    #[error("Entered unreachable code.")]
    Unreachable,
}

impl ParseError {
    pub fn print(self) {
        eprintln!("{}", format!("{}", self).red().bold());
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Statement,
    Assign,
    LessThanGreaterThan,
    Equals,
    Sum,
    Product,
    Exponential,
    Prefix,
    Call,
}

impl Precedence {
    fn token(token: Token) -> Self {
        match token {
            Token::Asterisk | Token::Slash => Self::Product,
            Token::Plus | Token::Minus => Self::Sum,
            Token::LeftParen | Token::Dot | Token::LeftBracket => Self::Call,
            Token::LessThan | Token::GreaterThan | Token::LessThanEquals | Token::GreaterThanEquals => Self::LessThanGreaterThan,
            Token::Equals | Token::NotEquals => Self::Equals,
            Token::Assign => Self::Assign,
            Token::LeftBrace => Self::Statement,
            Token::Exponential => Self::Exponential,
            _ => Self::Lowest,
        }
    }
}

pub struct Parser<'p> {
    tokens: Iter<'p, Token>,
    current: Token,
    peek: Token,
}

impl<'p> Parser<'p> {
    pub fn new(tokens: Iter<'p, Token>) -> Self {
        let mut s = Self {
            current: Token::Eof,
            peek: Token::Eof,
            tokens: tokens,
        };

        s.read();
        s.read();

        s
    }

    fn read(&mut self) {
        self.current = self.peek.clone();
        self.peek = if let Some(token) = self.tokens.next() { token.clone() } else { Token::Eof };
    }

    fn next(&mut self) -> ParseResult<Option<Statement>> {
        if self.current == Token::Eof {
            return Ok(None);
        }

        Ok(Some(self.parse_statement()?))
    }

    fn current_is(&self, token: Token) -> bool {
        std::mem::discriminant(&self.current) == std::mem::discriminant(&token)
    }

    fn expect_token(&mut self, token: Token) -> ParseResult<Token> {
        if self.current_is(token) {
            Ok(self.current.clone())
        } else {
            Err(ParseError::UnexpectedToken(self.current.clone()))
        }
    }

    fn expect_token_and_read(&mut self, token: Token) -> ParseResult<Token> {
        let result = self.expect_token(token)?;

        self.read();

        Ok(result)
    }

    fn expect_identifier_and_read(&mut self) -> ParseResult<Token> {
        self.expect_token_and_read(Token::Identifier("".to_string()))
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.current {
            Token::Fn => self.parse_fn(true),
            Token::Let => self.parse_let(),
            Token::If => self.parse_if(),
            Token::Return => {
                self.expect_token_and_read(Token::Return)?;

                if let Ok(expression) = self.parse_expression(Precedence::Lowest) {
                    Ok(Statement::Return { value: expression })
                } else {
                    Ok(Statement::Return { value: Expression::Null })
                }
            },
            _ => Ok(Statement::Expression { expression: self.parse_expression(Precedence::Lowest)? }),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParseResult<Expression> {
        let mut left = match self.current.clone() {
            Token::String(s) => {
                self.expect_token_and_read(Token::String("".to_string()))?;
                Expression::String(s)
            },
            Token::Nil => {
                self.expect_token_and_read(Token::Nil)?;
                Expression::Null
            },
            Token::Integer(i) => {
                self.expect_token_and_read(Token::Integer(0))?;
                Expression::Integer(i)
            },
            Token::True => {
                self.expect_token_and_read(Token::True)?;
                Expression::Bool(true)
            },
            Token::False => {
                self.expect_token_and_read(Token::False)?;
                Expression::Bool(false)
            },
            Token::Identifier(s) => {
                self.expect_identifier_and_read()?;
                Expression::Identifier(s)
            },
            Token::Fn => {
                let (params, body) = match self.parse_fn(false)? {
                    Statement::FunctionDeclaration { params, body, .. } => (params, body),
                    _ => return Err(ParseError::Unreachable),
                };

                Expression::Closure(params, body)
            },
            t @ Token::Minus | t @ Token::Bang => {
                self.expect_token_and_read(t.clone())?;

                Expression::Prefix(Op::token(t), self.parse_expression(Precedence::Prefix)?.boxed())
            },
            Token::LeftBracket => {
                self.expect_token_and_read(Token::LeftBracket)?;
                let mut items = vec![];

                while !self.current_is(Token::RightBracket) {
                    items.push(self.parse_expression(Precedence::Lowest)?);

                    if self.current_is(Token::Comma) {
                        self.expect_token_and_read(Token::Comma)?;
                    }
                }

                self.expect_token_and_read(Token::RightBracket)?;

                Expression::List(items)
            },
            _ => return Err(ParseError::UnexpectedToken(self.current.clone())),
        };

        while !self.current_is(Token::Eof) && precedence < Precedence::token(self.current.clone()) {
            if let Some(expression) = self.parse_postfix_expression(left.clone())? {
                left = expression;
            } else if let Some(expression) = self.parse_infix_expression(left.clone())? {
                left = expression;
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_postfix_expression(&mut self, left: Expression) -> ParseResult<Option<Expression>> {
        Ok(match self.current {
            Token::LeftBracket => {
                self.expect_token_and_read(Token::LeftBracket)?;

                let index: Option<Box<Expression>> = if self.current_is(Token::RightBracket) {
                    None
                } else {
                    Some(self.parse_expression(Precedence::Lowest)?.boxed())
                };

                self.expect_token_and_read(Token::RightBracket)?;

                Some(Expression::Index(left.boxed(), index))
            },
            Token::LeftParen => {
                self.expect_token_and_read(Token::LeftParen)?;

                let mut args = vec![];

                while !self.current_is(Token::RightParen) {
                    args.push(self.parse_expression(Precedence::Lowest)?);

                    if self.current_is(Token::Comma) {
                        self.read();
                    }
                }

                self.expect_token_and_read(Token::RightParen)?;

                Some(Expression::Call(Box::new(left), args))
            },
            _ => None,
        })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> ParseResult<Option<Expression>> {
        Ok(match self.current {
            Token::Plus | Token::Minus | Token::Asterisk | Token::Slash |
            Token::Equals | Token::NotEquals | Token::LessThanEquals | Token::LessThan |
            Token::GreaterThan | Token::GreaterThanEquals | Token::Exponential => {
                let token = self.current.clone();

                self.read();

                let right = self.parse_expression(Precedence::token(token.clone()))?;

                Some(Expression::Infix(Box::new(left), Op::token(token), Box::new(right)))
            },
            Token::Assign => {
                self.read();

                let right = self.parse_expression(Precedence::Lowest)?;

                Some(Expression::Assign(Box::new(left), Box::new(right)))
            },
            _ => None,
        })
    }

    fn parse_if(&mut self) -> ParseResult<Statement> {
        self.expect_token_and_read(Token::If)?;

        let condition = self.parse_expression(Precedence::Statement)?;
        let then = self.parse_block()?;
        let otherwise = if self.current_is(Token::Else) {
            self.expect_token_and_read(Token::Else)?;
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Statement::If { condition, then, otherwise })
    }

    fn parse_let(&mut self) -> ParseResult<Statement> {
        self.expect_token_and_read(Token::Let)?;

        let name: Identifier = self.expect_identifier_and_read()?.into();
        let initial = if self.current_is(Token::Assign) {
            self.expect_token_and_read(Token::Assign)?;

            Some(self.parse_expression(Precedence::Lowest)?)
        } else {
            None
        };

        Ok(Statement::LetDeclaration { name, initial })
    }

    fn parse_fn(&mut self, with_identifier: bool) -> ParseResult<Statement> {
        self.expect_token_and_read(Token::Fn)?;

        let name: Identifier = if with_identifier {
            self.expect_identifier_and_read()?.into()
        } else {
            String::from("<Closure>")
        };

        self.expect_token_and_read(Token::LeftParen)?;

        let mut params = vec![];

        while !self.current_is(Token::RightParen) {
            if self.current_is(Token::Comma) {
                self.expect_token_and_read(Token::Comma)?;
            }

            let param: String = self.expect_identifier_and_read()?.into();

            params.push(Parameter { name: param });
        }

        self.expect_token_and_read(Token::RightParen)?;

        let body = self.parse_block()?;

        Ok(Statement::FunctionDeclaration {
            name,
            params,
            body,
        })
    }

    fn parse_block(&mut self) -> ParseResult<Block> {
        self.expect_token_and_read(Token::LeftBrace)?;

        let mut block = vec![];

        while !self.current_is(Token::RightBrace) {
            block.push(self.parse_statement()?);
        }

        self.expect_token_and_read(Token::RightBrace)?;

        Ok(block)
    }
}
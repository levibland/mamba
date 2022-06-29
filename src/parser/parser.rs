use crate::lexer::Lexer;
use crate::lexer::token::Token;
use crate::parser::ast::{
    BlockStmt, 
    Expr, 
    Stmt, 
    Program, 
    Precedence, 
    Ident, 
    Literal, 
    Prefix, 
    Infix,
};

use std::mem;

type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug)]
enum ParserError {
    ExpectedIdent(Token),
    ExpectedAssign(Token),
    ExpectedLParen(Token),
    ExpectedRParen(Token),
    ExpectedLBrace(Token),
    ExpectedRBracket(Token),
    ExpectedColon(Token),
    ExpectedComma(Token),
    ExpectedToken { expected: Token, got: Token },
    ExpectedRBrace(Token),
    ExpectedPrefixToken(Token),
    ExpectedInfixToken(Token),
    ExpectedLiteral(Expr),
}

pub struct Parser {
    lexer: Lexer,
    cur_tok: Token,
    peek_tok: Token,
    errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            cur_tok: Token::Eof,
            peek_tok: Token::Eof,
            errors: vec![],
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.cur_tok = mem::replace(&mut self.peek_tok, self.lexer.next_token());
    }

    fn expect_peek<F>(&mut self, token: Token, parser_error: F) -> ParserResult<()> 
    where
        F: Fn(Token) -> ParserError,
    {
        let peek = self.peek_tok.clone();
        if peek == token {
            self.next_token();
            Ok(())
        } else {
            Err(parser_error(peek))
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.cur_tok != Token::Eof {
            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(error) => self.errors.push(error),
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> ParserResult<Stmt> {
        match self.cur_tok {
            Token::Let => self.parse_let_statement(),
            Token::Fn => self.parse_fn_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> ParserResult<Stmt> {
        let stmt = Stmt::ExprStmt(self.parse_expression(Precedence::Lowest)?);

        if self.peek_tok == Token::Semicolon {
            self.next_token();
        }

        Ok(stmt)
    }

    fn parse_expression(&mut self, prec: Precedence) -> ParserResult<Expr> {
        let mut left_exp = self.parse_prefix()?;

        while self.peek_tok != Token::Semicolon && prec < Precedence::from(&self.peek_tok) {
            left_exp = match Precedence::from(&self.peek_tok) {
                Precedence::Equals
                | Precedence::LessGreater
                | Precedence::Sum
                | Precedence::Product => {
                    self.next_token();
                    self.parse_infix_expression(Box::new(left_exp))?
                },
                Precedence::Call => {
                    self.next_token();
                    self.parse_call_expression(Box::new(left_exp))?
                },
                Precedence::Index => {
                    self.next_token();
                    self.parse_index_expression(Box::new(left_exp))?
                },
                _ => break,
            };
        }

        Ok(left_exp)
    }

    fn parse_prefix(&mut self) -> ParserResult<Expr> {
        match self.cur_tok.clone() {
            Token::Ident(i) => Ok(Expr::IdentExpr(Ident(i))),
            Token::Number(n) => Ok(Expr::LitExpr(Literal::NumberLiteral(n))),
            Token::String(s) => Ok(Expr::LitExpr(Literal::StringLiteral(s))),
            Token::Boolean(b) => Ok(Expr::LitExpr(Literal::BoolLiteral(b))),
            Token::LBracket => self.parse_array_literal(),
            Token::Bang | Token::Minus | Token::Plus => self.parse_prefix_expression(),
            Token::LParen => self.parse_grouped_expression(),
            Token::LBrace => self.parse_map_literal(),
            Token::If => self.parse_if_expression(),
            _ => Err(ParserError::ExpectedPrefixToken(self.cur_tok.clone())),
        }
    }

    fn parse_prefix_expression(&mut self) -> ParserResult<Expr> {
        let cur_tok = self.cur_tok.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        let prefix = match &cur_tok {
            Token::Bang => Prefix::Not,
            Token::Minus => Prefix::PrefixMinus,
            Token::Plus => Prefix::PrefixPlus,
            _ => return Err(ParserError::ExpectedPrefixToken(cur_tok)),
        };

        Ok(Expr::PrefixExpr(prefix, Box::new(right)))
    }

    fn parse_infix_expression(&mut self, left: Box<Expr>) -> ParserResult<Expr> {
        let token = self.cur_tok.clone();

        let right = {
            let prec = Precedence::from(&token);
            self.next_token();
            let expr = self.parse_expression(prec)?;
            Box::new(expr)
        };

        let infix = match &token {
            Token::Plus => Infix::Plus,
            Token::Minus => Infix::Minus,
            Token::Slash => Infix::Divide,
            Token::Asterisk => Infix::Multiply,
            Token::Eq => Infix::Equal,
            Token::NotEq => Infix::NotEqual,
            Token::GtEq => Infix::GreaterThanEqual,
            Token::LtEq => Infix::LessThanEqual,
            Token::Gt => Infix::GreaterThan,
            Token::Lt => Infix::LessThan,
            _ => return Err(ParserError::ExpectedInfixToken(token)),
        };

        Ok(Expr::InfixExpr(infix, left, right))
    }

    fn parse_call_expression(&mut self, left: Box<Expr>) -> ParserResult<Expr> {
        let args = self.parse_expression_list(Token::RParen)?;
        Ok(Expr::CallExpr { function: left, args })
    }


    fn parse_array_literal(&mut self) -> ParserResult<Expr> {
        let items = self.parse_expression_list(Token::RBracket)?;
        Ok(Expr::ArrayExpr(items))
    }

    fn parse_index_expression(&mut self, left: Box<Expr>) -> ParserResult<Expr> {
        // cur_tok: LBracket
        self.next_token();
        let index = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.expect_peek(Token::RBracket, ParserError::ExpectedRBracket)?;

        Ok(Expr::IndexExpr { array: left, index })
    }

    fn parse_expression_list(&mut self, end: Token) -> ParserResult<Vec<Expr>> {
        let mut items = vec![];

        self.next_token();
        if self.cur_tok != end {
            items.push(self.parse_expression(Precedence::Lowest)?);

            while self.peek_tok == Token::Comma {
                self.next_token();
                self.next_token();
                items.push(self.parse_expression(Precedence::Lowest)?);
            }

            self.expect_peek(end.clone(), |got| ParserError::ExpectedToken {
                expected: end.clone(),
                got,
            })?;
        }

        Ok(items)
    }

    fn parse_grouped_expression(&mut self) -> ParserResult<Expr> {
        self.next_token();
        let expr = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::RParen, ParserError::ExpectedRParen)?;

        Ok(expr)
    }

    fn parse_map_literal(&mut self) -> ParserResult<Expr> {
        // cur_tok: LBrace, peek_tok: beginning of expr or RBrace
        let mut pairs = vec![];

        while self.peek_tok != Token::RBrace {
            self.next_token();
            let key = self.parse_expression(Precedence::Lowest)?;

            let literal = match key {
                Expr::LitExpr(l) => l,
                _ => return Err(ParserError::ExpectedLiteral(key)),
            };

            self.expect_peek(Token::Colon, ParserError::ExpectedColon)?;

            // cur_tok: colon, peek_tok: beginning of expr
            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;
            pairs.push((literal, value));

            if self.peek_tok == Token::RBrace {
                break;
            }

            self.expect_peek(Token::Comma, ParserError::ExpectedComma)?;
        }

        self.expect_peek(Token::RBrace, ParserError::ExpectedRBrace)?;

        Ok(Expr::HashExpr(pairs))
    }

    fn parse_if_expression(&mut self) -> ParserResult<Expr> {
        // cur_tok: If, peek_tok: LParen
        self.expect_peek(Token::LParen, ParserError::ExpectedLParen)?;
        self.next_token(); // jump over LParen to get expression

        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.expect_peek(Token::RParen, ParserError::ExpectedRParen)?;
        self.expect_peek(Token::LBrace, ParserError::ExpectedLBrace)?;

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_tok == Token::Else {
            self.next_token();
            self.expect_peek(Token::LBrace, ParserError::ExpectedLBrace)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expr::IfExpr { condition, consequence, alternative })
    }

    fn parse_block_statement(&mut self) -> ParserResult<BlockStmt> {
        let mut block = BlockStmt::new();
        self.next_token();

        while self.cur_tok != Token::RBrace && self.cur_tok != Token::Eof {
            let stmt = self.parse_statement()?;
            block.statements.push(stmt);
            self.next_token();
        }

        Ok(block)
    }

    fn parse_let_statement(&mut self) -> ParserResult<Stmt> {
        // cur_tok: Let, peek_tok: Ident
        let identifier: String;

        if let Token::Ident(ident) = self.peek_tok.clone() {
            identifier = ident;
            self.next_token();
        } else {
            return Err(ParserError::ExpectedIdent(self.peek_tok.clone()))
        }

        // cur_tok: Ident, peek_tok: Assign
        self.expect_peek(Token::Assign, ParserError::ExpectedAssign)?;

        // cur_tok: Assign, peek_tok: beginning of expr
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_tok == Token::Semicolon {
            self.next_token();
        }

        Ok(Stmt::LetStmt(Ident(identifier), value))
    }

    fn parse_return_statement(&mut self) -> ParserResult<Stmt> {
        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest)?;
        
        if self.peek_tok == Token::Semicolon {
            self.next_token();
        }

        Ok(Stmt::ReturnStmt(return_value))
    }

    fn parse_fn_statement(&mut self) -> ParserResult<Stmt> {
        // cur_tok: Fn, peek_tok: Ident
        let identifier: String;

        if let Token::Ident(ident) = self.peek_tok.clone() {
            identifier = ident;
            self.next_token();
        } else {
            return Err(ParserError::ExpectedIdent(self.peek_tok.clone()))
        }

        // cur_tok: Ident, peek_tok: LParen
        self.expect_peek(Token::LParen, ParserError::ExpectedLParen)?;

        // get params
        let params = self.parse_function_parameters()?;

        self.expect_peek(Token::LBrace, ParserError::ExpectedLBrace)?;

        let body = self.parse_block_statement()?;

        Ok(Stmt::FnStmt(Ident(identifier), params, body))
    }

    fn parse_function_parameters(&mut self) -> ParserResult<Vec<Ident>> {
        let mut parameters = vec![];

        self.next_token();
        if self.cur_tok == Token::RParen {
            return Ok(parameters);
        }

        let ident = match self.cur_tok.clone() {
            Token::Ident(i) => Ident(i),
            _ => return Err(ParserError::ExpectedIdent(self.cur_tok.clone())),
        };
        parameters.push(ident);

        while self.peek_tok == Token::Comma {
            self.next_token();
            self.next_token();
            let ident = match self.cur_tok.clone() {
                Token::Ident(i) => Ident(i),
                _ => return Err(ParserError::ExpectedIdent(self.cur_tok.clone())),
            };
            parameters.push(ident);
        }

        self.expect_peek(Token::RParen, ParserError::ExpectedRParen)?;

        Ok(parameters)
    }

    pub fn check_parser_errors(&self) {
        if self.errors.is_empty() {
            return;
        }

        eprintln!("Parser has {} errors:", self.errors.len());
        for (i, error) in self.errors.iter().enumerate() {
            eprintln!("\t{}. {:?}", i, error);
        }
    }
}

#[cfg(test)]
mod parser_test_precedence {
    use super::*;

    #[test]
    fn test_ord() {
        assert!(Precedence::Lowest < Precedence::Equals);
        assert!(Precedence::Equals > Precedence::Lowest);
    }
}

#[cfg(test)]
mod parser_test_statements {
    use super::*;

    #[test]
    fn test_let_statement() {
        let input = r"let x = 1; let y = true; let z = y;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected = vec![
            Stmt::LetStmt(Ident("x".to_string()), Expr::LitExpr(Literal::NumberLiteral(1f64))),
            Stmt::LetStmt(Ident("y".to_string()), Expr::LitExpr(Literal::BoolLiteral(true))),
            Stmt::LetStmt(Ident("z".to_string()), Expr::IdentExpr(Ident("y".to_string()))),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_return_statement() {
        let input = r"return 7; return 3 + 4;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected = vec![
            Stmt::ReturnStmt(Expr::LitExpr(Literal::NumberLiteral(7f64))),
            Stmt::ReturnStmt(Expr::InfixExpr(
                Infix::Plus,
                Box::new(Expr::LitExpr(Literal::NumberLiteral(3f64))),
                Box::new(Expr::LitExpr(Literal::NumberLiteral(4f64)))
            )),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_fn_statement() {
        let input = r"fn test(x, y) { x + y; }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let blockstmt = BlockStmt {
            statements: vec![Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Plus,
                Box::new(Expr::IdentExpr(Ident("x".to_string()))),
                Box::new(Expr::IdentExpr(Ident("y".to_string()))),
            ))],
        };

        let expected = vec![
            Stmt::FnStmt(
                Ident("test".to_string()),
                vec![Ident("x".to_string()), Ident("y".to_string())],
                blockstmt,
            )
        ];

        assert_eq!(program.statements, expected);
    }
}
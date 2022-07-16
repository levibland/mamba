use std::fmt;
use mamba_lexer::token::*;

pub struct Program {
    pub statements: Vec<Stmt>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s: String = self.statements.iter().map(|x| x.to_string()).collect();
        write!(f, "{}", &s)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    LetStmt(Ident, Expr),
    ReturnStmt(Expr),
    ExprStmt(Expr),
    FnStmt(Ident, Vec<Ident>, BlockStmt),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::LetStmt(i, e) => write!(f, "let {} = {};", i, e),
            Stmt::ReturnStmt(e) => write!(f, "return {};", e),
            Stmt::ExprStmt(e) => write!(f, "{}", e),
            Stmt::FnStmt(ident, args, body) => write!(f, "fn {}({}) {}", ident, csv_str(args), body),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    IdentExpr(Ident),
    LitExpr(Literal),
    PrefixExpr(Prefix, Box<Expr>),
    InfixExpr(Infix, Box<Expr>, Box<Expr>),
    IfExpr {
        condition: Box<Expr>,
        consequence: BlockStmt,
        alternative: Option<BlockStmt>,
    },
    CallExpr {
        function: Box<Expr>,
        args: Vec<Expr>,
    },
    ArrayExpr(Vec<Expr>),
    HashExpr(Vec<(Literal, Expr)>),
    IndexExpr {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    ClosureExpr {
        params: Vec<Ident>,
        body: BlockStmt,
    },
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::IdentExpr(i) => write!(f, "{}", i),
            Expr::LitExpr(l) => write!(f, "{}", l),
            Expr::PrefixExpr(p, e) => write!(f, "{}{}", p, e),
            Expr::InfixExpr(i, rhs, lhs) => write!(f, "{} {} {}", rhs, i, lhs),
            Expr::IfExpr { condition, consequence, alternative } => match alternative {
                Some(alt) => write!(f, "if {} {} else {}", condition, consequence, alt),
                None => write!(f, "if {} {}", condition, consequence),
            },
            Expr::CallExpr { function, args } => write!(f, "{}({})", function, csv_str(args)),
            Expr::ArrayExpr(a) => write!(f, "[{}]", csv_str(a)),
            Expr::HashExpr(map) => {
                let pairs: Vec<String> = map.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
                let pairs = csv_str(&pairs);
                write!(f, "{{{}}}", pairs)
            },
            Expr::IndexExpr { array, index } => write!(f, "({}[{}])", array, index),
            Expr::ClosureExpr { params, body } => write!(f, "|{}| {}", csv_str(params), body),
        }
    }
}

pub fn csv_str<T: fmt::Display>(arr: &[T]) -> String {
    arr.iter()
        .map(|e| e.to_string())
        .collect::<Vec<String>>()
        .join(", ")
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    NumberLiteral(i64),
    BoolLiteral(bool),
    StringLiteral(String),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::NumberLiteral(n) => write!(f, "{}", n),
            Literal::BoolLiteral(b) => write!(f, "{}", b),
            Literal::StringLiteral(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident(pub String);

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Prefix {
    PrefixPlus,
    PrefixMinus,
    Not,
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Prefix::PrefixPlus => write!(f, "+"),
            Prefix::PrefixMinus => write!(f, "-"),
            Prefix::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Infix {
    Plus,
    Minus,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    GreaterThanEqual,
    LessThanEqual,
    GreaterThan,
    LessThan,
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Divide => write!(f, "/"),
            Infix::Multiply => write!(f, "*"),
            Infix::Equal => write!(f, "=="),
            Infix::NotEqual => write!(f, "!="),
            Infix::GreaterThanEqual => write!(f, ">="),
            Infix::LessThanEqual => write!(f, "<="),
            Infix::GreaterThan => write!(f, ">"),
            Infix::LessThan => write!(f, "<"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

impl From<&Token> for Precedence {
    fn from(token: &Token) -> Self {
        match token {
            Token::Eq | Token::NotEq => Precedence::Equals,
            Token::Lt | Token::Gt | Token::GtEq | Token::LtEq => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Asterisk | Token::Slash => Precedence::Product,
            Token::Open(Delimiter::Paren) => Precedence::Call,
            Token::Open(Delimiter::Brack) => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}

impl BlockStmt {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }
}

impl fmt::Display for BlockStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s: String = self.statements.iter().map(|x| x.to_string()).collect();
        write!(f, "{}", &s)
    }
}
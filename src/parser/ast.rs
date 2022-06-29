pub type Program = Vec<Stmt>;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    LetStmt(Ident, Expr),
    ReturnStmt(Expr),
    ExprStmt(Expr),
    FnStmt(Ident, Vec<Ident>, Program),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    IdentExpr(Ident),
    LitExpr(Literal),
    PrefixExpr(Prefix, Box<Expr>),
    InfixExpr(Infix, Box<Expr>, Box<Expr>),
    IfExpr {
        condition: Box<Expr>,
        consequence: Program,
        alternative: Option<Program>,
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    NumberLiteral(f64),
    BoolLiteral(bool),
    StringLiteral(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident(pub String);

#[derive(Debug, Clone, PartialEq)]
pub enum Prefix {
    PrefixPlus,
    PrefixMinus,
    Not,
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

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Call,
    Index,
}
use lexer::Token;

pub type Program = Vec<Statement>;
pub type Block = Vec<Statement>;
pub type Identifier = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Return {
        value: Expression,
    },
    FunctionDeclaration {
        name: Identifier,
        params: Vec<Parameter>,
        body: Block,
    },
    LetDeclaration {
        name: Identifier,
        initial: Option<Expression>,
    },
    If {
        condition: Expression,
        then: Block,
        otherwise: Option<Block>,
    },
    Expression {
        expression: Expression,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Integer(i64),
    String(String),
    Bool(bool),
    Null,
    Identifier(Identifier),
    Assign(Box<Expression>, Box<Expression>),
    Infix(Box<Expression>, Op, Box<Expression>),
    Prefix(Op, Box<Expression>),
    Call(Box<Expression>),
    Closure(Vec<Parameter>, Vec<Statement>),
    Index(Box<Expression>, Option<Box<Expression>>),
    List(Vec<Expression>),
}

impl Expression {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Bang,
    Equals,
    NotEquals,
    Assign,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    Exponential,
}

impl Op {
    pub fn token(token: Token) -> Self {
        match token {
            Token::Plus => Self::Add,
            Token::Minus => Self::Subtract,
            Token::Asterisk => Self::Multiply,
            Token::Slash => Self::Divide,
            Token::Bang => Self::Bang,
            Token::Percent => Self::Modulo,
            Token::Equals => Self::Equals,
            Token::NotEquals => Self::NotEquals,
            Token::Assign => Self::Assign,
            Token::LessThan => Self::LessThan,
            Token::GreaterThan => Self::GreaterThan,
            Token::LessThanEquals => Self::LessThanEquals,
            Token::GreaterThanEquals => Self::GreaterThanEquals,
            Token::Exponential => Self::Exponential,
            _ => unreachable!("{:?}", token)
        }
    }
}
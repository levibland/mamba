use crate::environment::*;
use crate::object::*;
use mamba_parser::ast::*;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    fn returned(&mut self, object: Object) -> Object {
        match object {
            Object::ReturnValue(v) => *v,
            o => o,
        }
    }

    pub fn eval_program(&mut self, prog: Program) -> Object {
        let return_data = self.eval_blockstmt(prog);
        self.returned(return_data)
    }

    pub fn eval_blockstmt(&mut self, mut prog: Program) -> Object {
        match prog.statements.len() {
            0 => Object::Null,
            1 => self.eval_statement(prog.statements.remove(0)),
            _ => {
                let s = prog.statements.remove(0);
                let object = self.eval_statement(s);
                if object.is_returned() {
                    object
                } else {
                    self.eval_blockstmt(prog)
                }
            }
        }
    }

    pub fn eval_statement(&mut self, stmt: Stmt) -> Object {
        match stmt {
            Stmt::ExprStmt(expr) => self.eval_expr(expr),
            Stmt::ReturnStmt(expr) => Object::ReturnValue(Box::new(self.eval_expr(expr))),
            Stmt::LetStmt(ident, expr) => {
                let object = self.eval_expr(expr);
                self.register_ident(ident, object)
            },
            Stmt::FnStmt(ident, params, body) => {
                let object = Object::Function(params, body.statements, Rc::clone(&self.env));
                self.register_ident(ident, object)
            }
        }
    }

    pub fn register_ident(&mut self, ident: Ident, object: Object) -> Object {
        let Ident(name) = ident;
        self.env.borrow_mut().set(&name, object.clone());
        object
    }

    pub fn eval_expr(&mut self, expr: Expr) -> Object {
        match expr {
            Expr::IdentExpr(i) => self.eval_ident(i),
            Expr::LitExpr(l) => self.eval_literal(l),
            Expr::PrefixExpr(prefix, expr) => self.eval_prefix(&prefix, *expr),
            Expr::InfixExpr(infix, expr1, expr2) => self.eval_infix(&infix, *expr1, *expr2),
            Expr::IfExpr {
                condition, 
                consequence, 
                alternative,
            } => self.eval_if(*condition, consequence, alternative),
            Expr::CallExpr {
                function,
                args,
            } => self.eval_call(*function, args),
            Expr::ArrayExpr(exprs) => self.eval_array(exprs),
            Expr::HashExpr(hash_exprs) => self.eval_hash(hash_exprs),
            Expr::IndexExpr { array, index } => self.eval_index(*array, *index),
            Expr::ClosureExpr {
                params, 
                body 
            } => Object::Function(params, body.statements, Rc::clone(&self.env)),
        }
    }

    pub fn eval_ident(&mut self, ident: Ident) -> Object {
        let Ident(name) = ident;
        let borrow_env = self.env.borrow();
        let var = borrow_env.get(&name);
        match var {
            Some(o) => o,
            None => Object::Error(format!("identifier not found: {}", name)),
        }
    }

    pub fn eval_literal(&mut self, literal: Literal) -> Object {
        match literal {
            Literal::NumberLiteral(n) => Object::Number(n),
            Literal::BoolLiteral(b) => Object::Boolean(b),
            Literal::StringLiteral(s) => Object::String(s),
        }
    }

    pub fn eval_prefix(&mut self, prefix: &Prefix, expr: Expr) -> Object {
        let object = self.eval_expr(expr);
        match *prefix {
            Prefix::Not => match self.object_to_bool(object) {
                Ok(b) => Object::Boolean(!b),
                Err(err) => err,
            },
            Prefix::PrefixPlus => match self.object_to_number(object) {
                Ok(n) => Object::Number(n),
                Err(err) => err,
            },
            Prefix::PrefixMinus => match self.object_to_number(object) {
                Ok(n) => Object::Number(-n),
                Err(err) => err,
            },
        }
    }

    pub fn eval_infix(&mut self, infix: &Infix, expr1: Expr, expr2: Expr) -> Object {
        let object1 = self.eval_expr(expr1);
        let object2 = self.eval_expr(expr2);
        match *infix {
            Infix::Plus => self.object_add(object1, object2),
            Infix::Minus => {
                let n1 = self.object_to_number(object1);
                let n2 = self.object_to_number(object2);
                match (n1, n2) {
                    (Ok(n1), Ok(n2)) => Object::Number(n1 - n2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            },
            Infix::Divide => {
                let n1 = self.object_to_number(object1);
                let n2 = self.object_to_number(object2);
                match (n1, n2) {
                    (Ok(n1), Ok(n2)) => Object::Number(n1 / n2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            },
            Infix::Multiply => {
                let n1 = self.object_to_number(object1);
                let n2 = self.object_to_number(object2);
                match (n1, n2) {
                    (Ok(n1), Ok(n2)) => Object::Number(n1 * n2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            },
            Infix::Equal => Object::Boolean(object1 == object2),
            Infix::NotEqual => Object::Boolean(object1 != object2),
            Infix::GreaterThanEqual => {
                let n1 = self.object_to_number(object1);
                let n2 = self.object_to_number(object2);
                match (n1, n2) {
                    (Ok(n1), Ok(n2)) => Object::Boolean(n1 >= n2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            },
            Infix::GreaterThan => {
                let n1 = self.object_to_number(object1);
                let n2 = self.object_to_number(object2);
                match (n1, n2) {
                    (Ok(n1), Ok(n2)) => Object::Boolean(n1 > n2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            },
            Infix::LessThanEqual => {
                let n1 = self.object_to_number(object1);
                let n2 = self.object_to_number(object2);
                match (n1, n2) {
                    (Ok(n1), Ok(n2)) => Object::Boolean(n1 <= n2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            },
            Infix::LessThan => {
                let n1 = self.object_to_number(object1);
                let n2 = self.object_to_number(object2);
                match (n1, n2) {
                    (Ok(n1), Ok(n2)) => Object::Boolean(n1 < n2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            },
        }
    }

    pub fn eval_if(&mut self, cond: Expr, conse: BlockStmt, maybe_alter: Option<BlockStmt>) -> Object {
        let object = self.eval_expr(cond);
        let prog = Program { statements: conse.statements };
        match self.object_to_bool(object) {
            Ok(b) => {
                if b {
                    self.eval_blockstmt(prog)
                } else {
                    match maybe_alter {
                        Some(else_conse) => {
                            let prog = Program { statements: else_conse.statements };
                            self.eval_blockstmt(prog)
                        },
                        _ => Object::Null,
                    }
                }
            },
            Err(err) => err,
        }
    }

    pub fn eval_call(&mut self, fn_expr: Expr, args_expr: Vec<Expr>) -> Object {
        let fn_object = self.eval_expr(fn_expr);
        let fn_ = self.object_to_function(fn_object);
        match fn_ {
            Object::Function(params, body, f_env) => {
                self.eval_fn_call(args_expr, params, body, &f_env)
            },
            Object::Builtin(_, num_params, b_fn) => {
                self.eval_builtin_call(args_expr, num_params, b_fn)
            },
            o_err => o_err,
        }
    }

    fn eval_fn_call(
        &mut self,
        args_expr: Vec<Expr>,
        params: Vec<Ident>,
        body: Vec<Stmt>,
        f_env: &Rc<RefCell<Environment>>,
    ) -> Object {
        if args_expr.len() != params.len() {
            Object::Error(format!(
                "wrong number of input arguments: {} expected but {} given",
                params.len(),
                args_expr.len()
            ))
        } else {
            let args = args_expr
                .into_iter()
                .map(|e| self.eval_expr(e))
                .collect::<Vec<_>>();

            let old_env = Rc::clone(&self.env);

            let mut new_env = Environment::new_with_outer(Rc::clone(f_env));

            let zipped = params.into_iter().zip(args);

            for (_, (Ident(name), o)) in zipped.enumerate() {
                new_env.set(&name, o);
            }

            self.env = Rc::new(RefCell::new(new_env));

            let object = self.eval_blockstmt(Program { statements: body });

            self.env = old_env;

            self.returned(object)
        }
    }

    fn eval_builtin_call(
        &mut self,
        args_expr: Vec<Expr>,
        num_params: usize,
        b_fn: BuiltinFunction,
    ) -> Object {
        if args_expr.len() != num_params {
            Object::Error(format!(
                "wrong number of arguments: {} expected but {} given",
                num_params,
                args_expr.len()
            ))
        } else {
            let args = args_expr
                .into_iter()
                .map(|e| self.eval_expr(e))
                .collect::<Vec<_>>();
            b_fn(args).unwrap_or_else(Object::Error)
        }
    }

    pub fn eval_array(&mut self, exprs: Vec<Expr>) -> Object {
        let new_vec = exprs.into_iter().map(|e| self.eval_expr(e)).collect();
        Object::Array(new_vec)
    }

    pub fn object_add(&mut self, object1: Object, object2: Object) -> Object {
        match (object1, object2) {
            (Object::Number(n1), Object::Number(n2)) => Object::Number(n1 + n2),
            (Object::String(s1), Object::String(s2)) => Object::String(s1 + &s2),
            (Object::Error(s), _) | (_, Object::Error(s)) => Object::Error(s),
            (x, y) => Object::Error(format!("{:?} and {:?} are not addable", x, y)),
        }
    }

    pub fn eval_hash(&mut self, hs: Vec<(Literal, Expr)>) -> Object {
        let hashmap = hs.into_iter().map(|pair| self.eval_pair(pair)).collect();
        Object::Hash(hashmap)
    }

    fn eval_pair(&mut self, tuple: (Literal, Expr)) -> (Object, Object) {
        let (l, e) = tuple;
        let hash = self.literal_to_hash(l);
        let object = self.eval_expr(e);
        (hash, object)
    }

    pub fn eval_index(&mut self, target_exp: Expr, id_exp: Expr) -> Object {
        let target = self.eval_expr(target_exp);
        let index = self.eval_expr(id_exp);
        match target {
            Object::Array(arr) => match self.object_to_number(index) {
                Ok(index_number) => arr
                    .into_iter()
                    .nth(index_number as usize)
                    .unwrap_or(Object::Null),
                Err(err) => err,
            },
            Object::Hash(mut hash) => {
                let name = self.object_to_hash(index);
                match name {
                    Object::Error(_) => name,
                    _ => hash.remove(&name).unwrap_or(Object::Null),
                }
            }
            o => Object::Error(format!("unexpected index target: {}", o)),
        }
    }

    pub fn object_to_bool(&mut self, object: Object) -> Result<bool, Object> {
        match object {
            Object::Boolean(b) => Ok(b),
            Object::Error(s) => Err(Object::Error(s)),
            b => Err(Object::Error(format!("{} is not a bool", b))),
        }
    }

    pub fn object_to_number(&mut self, object: Object) -> Result<i64, Object> {
        match object {
            Object::Number(n) => Ok(n),
            Object::Error(s) => Err(Object::Error(s)),
            n => Err(Object::Error(format!("{} is not a number", n))),
        }
    }

    pub fn object_to_function(&mut self, object: Object) -> Object {
        match object {
            Object::Function(_, _, _) | Object::Builtin(_, _, _) => object,
            Object::Error(s) => Object::Error(s),
            f => Object::Error(format!("{} is not a valid function", f)),
        }
    }

    pub fn object_to_hash(&mut self, object: Object) -> Object {
        match object {
            Object::Number(n) => Object::Number(n),
            Object::Boolean(b) => Object::Boolean(b),
            Object::String(s) => Object::String(s),
            Object::Error(s) => Object::Error(s),
            x => Object::Error(format!("{} is not hashable", x)),
        }
    }

    pub fn literal_to_hash(&mut self, literal: Literal) -> Object {
        let object = self.eval_literal(literal);
        self.object_to_hash(object)
    }
}

#[cfg(test)]
mod evaluator_tests {
    use super::*;
    use mamba_lexer::*;
    use mamba_parser::*;

    fn compare(input: &str, object: Object) {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let mut evaluator = Evaluator::new();
        let eval = evaluator.eval_program(program);
        assert_eq!(eval, object);
    }

    #[test]
    fn test_literals() {
        // numbers
        compare("5", Object::Number(5));
        compare("12", Object::Number(12));

        // booleans
        compare("true", Object::Boolean(true));
        compare("false", Object::Boolean(false));
    }

    #[test]
    fn test_prefix_ops() {
        // bang op
        compare("!false", Object::Boolean(true));
        compare("!true", Object::Boolean(false));
        compare("!!false", Object::Boolean(false));
        compare("!!true", Object::Boolean(true));
        compare(
            "!10",
            Object::Error("10 is not a bool".to_string()),
        );

        // + prefix
        compare("+1", Object::Number(1));
        compare("+100", Object::Number(100));
        compare(
            "+true",
            Object::Error("true is not a number".to_string()),
        );

        // - prefix
        compare("-1", Object::Number(-1));
        compare("-5", Object::Number(-5));
        compare(
            "-false",
            Object::Error("false is not a number".to_string()),
        );
    }
}
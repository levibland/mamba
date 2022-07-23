use std::slice::Iter;
use std::rc::Rc;
use std::cell::{RefCell, Ref, RefMut};
use std::path::PathBuf;
use std::fs::canonicalize;
use std::collections::HashMap;
use thiserror::Error;
use colored::*;
use ::parser::*;

use crate::environment::*;

pub type EvaluatorResult<T> = Result<T, EvaluatorResults>;

#[derive(Debug, Error)]
pub enum EvaluatorResults {
    #[error("")]
    Return(Value),

    #[error("Undefined variable: {0}.")]
    UndefinedVariable(String),

    #[error("Undefined index: {0}.")]
    UndefinedIndex(usize),

    #[error("Unable to iterate over value of type {0}.")]
    InvalidIterable(String),

    #[error("Too few arguments to function {0}(), {1} passed in, {2} expected.")]
    TooFewArguments(String, usize, usize),

    #[error("Cannot append to value of type {0}.")]
    InvalidAppendTarget(String),
}

impl EvaluatorResults {
    pub fn print(self) {
        eprintln!("{}", format!("{}", self).red().bold());
        std::process::exit(1);
    }
}

pub fn register_globals(evaluator: &mut Evaluator) {
    evaluator.define_global_function("println", crate::stdlib::println);
}

pub fn evaluate(ast: Program, path: PathBuf) -> EvaluatorResult<()> {
    let mut evaluator = Evaluator::new(ast.iter(), canonicalize(path).unwrap());

    register_globals(&mut evaluator);

    evaluator.run()
}

#[derive(Debug, Clone)]
pub struct Evaluator<'e> {
    ast: Iter<'e, Statement>,
    environment: Rc<RefCell<Environment>>,
    pub globals: HashMap<String, Value>,
    path: PathBuf,
}

impl<'e> Evaluator<'e> {
    pub fn new(ast: Iter<'e, Statement>, path: PathBuf) -> Self {
        Self {
            ast: ast,
            environment: Rc::new(RefCell::new(Environment::new())),
            globals: HashMap::new(),
            path: path,
        }
    }

    fn run_statement(&mut self, statement: Statement) -> EvaluatorResult<()> {
        Ok(match statement {
            Statement::LetDeclaration { name, initial } => {
                if initial.is_none() {
                    self.env_mut().set(name, Value::Nil)
                } else {
                    let initial = initial.unwrap();
                    let value = self.run_expression(initial)?;

                    self.env_mut().set(name, value)
                }
            },
            Statement::FunctionDeclaration { name, params, body } => {
                self.globals.insert(name.clone(), Value::Function {
                    name, params, body, environment: None, context: None,
                });
            },
            Statement::For { iterable, value, index, then } => {
                let iterable = self.run_expression(iterable)?;

                let items = match iterable {
                    Value::List(items) => items,
                    _ => return Err(EvaluatorResults::InvalidIterable(iterable.typestring())),
                };

                // if there aren't any items in the list, we can leave this execution
                // cycle early.
                if items.borrow().is_empty() {
                    return Ok(())
                }

                let set_index = index.is_some();

                for (i, item) in items.borrow().iter().enumerate() {
                    self.env_mut().set(value.clone(), item.clone());

                    if set_index {
                        self.env_mut().set(index.clone().unwrap(), Value::Integer(i as i64));
                    }

                    for statement in then.clone() {
                        self.run_statement(statement)?;
                    }
                }

                self.env_mut().drop(value);

                if set_index {
                    self.env_mut().drop(index.unwrap());
                }
            },
            Statement::If { condition, then, otherwise } => {
                let condition = self.run_expression(condition)?;

                if condition.to_bool() {
                    for statement in then {
                        self.run_statement(statement)?;
                    }
                } else if otherwise.is_some() {
                    for statement in otherwise.unwrap() {
                        self.run_statement(statement)?;
                    }
                }
            },
            Statement::Expression { expression } => {
                self.run_expression(expression)?;
            },
            Statement::Return { value } => {
                return Err(EvaluatorResults::Return(self.run_expression(value)?));
            },
        })
    }

    fn run_expression(&mut self, expression: Expression) -> EvaluatorResult<Value> {
        Ok(match expression {
            Expression::Integer(i) => Value::Integer(i),
            Expression::String(s) => Value::String(s),
            Expression::Bool(b) => Value::Bool(b),
            Expression::Identifier(s) => {
                if self.globals.contains_key(&s) {
                    self.globals[&s].clone()
                } else {
                    if let Some(v) = self.env().get(s.clone()) {
                        v
                    } else {
                        return Err(EvaluatorResults::UndefinedVariable(s));
                    }
                }
            },
            Expression::Index(target, index) => {
                let instance = self.run_expression(*target)?;
                let index = self.run_expression(*index.expect("Expected index."))?.to_int() as usize;

                match instance {
                    Value::List(items) => {
                        match items.borrow().get(index) {
                            Some(v) => v.clone(),
                            None => return Err(EvaluatorResults::UndefinedIndex(index)),
                        }
                    },
                    _ => unreachable!(),
                }
            },
            Expression::Infix(left, op, right) => {
                let left = self.run_expression(*left)?;
                let right = self.run_expression(*right)?;

                match (left, op, right) {
                    (Value::Integer(l), Op::Add, Value::Integer(r)) => Value::Integer(l + r),
                    (Value::Integer(l), Op::Multiply, Value::Integer(r)) => Value::Integer(l * r),
                    (Value::Integer(l), Op::Divide, Value::Integer(r)) => Value::Integer(l / r),
                    (Value::Integer(l), Op::Subtract, Value::Integer(r)) => Value::Integer(l - r),
                    (Value::Integer(l), Op::Add, Value::String(r)) => {
                        let mut l = l.to_string();
                        l.push_str(r.as_str());
                        Value::String(l)
                    },
                    (Value::String(l), Op::Add, Value::Integer(r)) => {
                        let mut l = l;
                        l.push_str(r.to_string().as_str());
                        Value::String(l)
                    },
                    (Value::String(l), Op::Add, Value::String(r)) => {
                        let mut l = l;
                        l.push_str(r.as_str());
                        Value::String(l)
                    },
                    (Value::String(l), Op::Equals, Value::String(r)) => Value::Bool(l == r),
                    (Value::Integer(l), Op::Equals, Value::Integer(r)) => Value::Bool(l == r),
                    (Value::Bool(l), Op::Equals, Value::Bool(r)) => Value::Bool(l == r),
                    (Value::String(l), Op::NotEquals, Value::String(r)) => Value::Bool(l != r),
                    (Value::Integer(l), Op::NotEquals, Value::Integer(r)) => Value::Bool(l != r),
                    (Value::Bool(l), Op::NotEquals, Value::Bool(r)) => Value::Bool(l != r),
                    (Value::Integer(l), Op::LessThan, Value::Integer(r)) => Value::Bool(l < r),
                    (Value::Integer(l), Op::GreaterThan, Value::Integer(r)) => Value::Bool(l > r),
                    (Value::Integer(l), Op::LessThanEquals, Value::Integer(r)) => Value::Bool(l <= r),
                    (Value::Integer(l), Op::GreaterThanEquals, Value::Integer(r)) => Value::Bool(l >= r),
                    (Value::Integer(l), Op::Exponential, Value::Integer(r)) => Value::Integer(l.pow(r as u32)),
                    /*(l, Op::In, Value::List(r)) => {
                        let filtered: Vec<Value> = r.borrow().clone()
                            .into_iter()
                            .filter(|v| {
                                v.clone().is(l.clone())
                            })
                            .collect();

                        Value::Bool(! filtered.is_empty())
                    },*//* 
                    (Value::String(l), Op::In, Value::String(r)) => {
                        Value::Bool(r.contains(l.as_str()))
                    },
                    (l, Op::NotIn, Value::List(r)) => {
                        let filtered: Vec<Value> = r.borrow().clone()
                            .into_iter()
                            .filter(|v| {
                                v.clone().is(l.clone())
                            })
                            .collect();

                        Value::Bool(filtered.is_empty())
                    },
                    (Value::String(l), Op::NotIn, Value::String(r)) => {
                        Value::Bool(! r.contains(l.as_str()))
                    },
                    */
                    _ => todo!(),
                }
            },
            Expression::List(items) => {
                let mut values: Vec<Value> = Vec::new();

                for item in items.into_iter() {
                    values.push(self.run_expression(item)?);
                }

                Value::List(Rc::new(RefCell::new(values)))
            },
            Expression::Closure(params, body) => {
                Value::Function {
                    name: String::from("Closure"),
                    params,
                    body,
                    environment: Some(self.environment.borrow().clone()),
                    context: None,
                }
            },
            Expression::Call(callable, arguments) => {
                let callable = self.run_expression(*callable)?;
                let mut argument_values: Vec<Value> = Vec::new();

                for argument in arguments.into_iter() {
                    argument_values.push(self.run_expression(argument)?);
                }

                self.call(callable, argument_values)?
            },
            Expression::Prefix(op, right) => {
                let right = self.run_expression(*right)?;

                match op {
                    Op::Bang => Value::Bool(! right.to_bool()),
                    Op::Subtract => Value::Integer(- right.to_int()),
                    _ => unreachable!()
                }
            },
            Expression::Assign(target, value) => {
                let value = self.run_expression(*value)?;

                fn assign_to_list(interpreter: &mut Evaluator, instance: Value, index: Option<Box<Expression>>, value: Value) -> EvaluatorResult<()> {
                    Ok(match instance {
                        Value::List(items) => {
                            match index {
                                Some(i) => {
                                    let index = interpreter.run_expression(*i)?.to_int();
                                    items.borrow_mut()[index as usize] = value.clone();
                                },
                                None => {
                                    items.borrow_mut().push(value.clone());
                                }
                            }
                        },
                        _ => return Err(EvaluatorResults::InvalidAppendTarget(instance.typestring()))
                    })
                }

                match *target.clone() {
                    Expression::Index(instance, index) => {
                        let instance = self.run_expression(*instance)?;

                        assign_to_list(self, instance, index, value.clone())?;
                    },
                    _ => {
                        match *target.clone() {
                            Expression::Identifier(i) => {
                                self.env_mut().set(i, value.clone());
                            },
                            _ => todo!()
                        }
                    }
                };

                value
            },
            _ => todo!("{:?}", expression),
        })
    }

    pub fn call(&mut self, callable: Value, arguments: Vec<Value>) -> EvaluatorResult<Value> {
        Ok(match callable {
            Value::NativeFunction { callback, .. } => callback(self, arguments),
            Value::Function { name, params, body, environment, context } => {
                if params.len() != arguments.len() {
                    return Err(EvaluatorResults::TooFewArguments(name.clone(), arguments.len(), params.len()));
                }

                let old_environment = Rc::clone(&self.environment);
                let new_environment = if environment.is_some() { 
                    Rc::new(RefCell::new(environment.unwrap()))
                } else {
                    Rc::new(RefCell::new(Environment::new()))
                };

                for (Parameter { name, .. }, value) in params.into_iter().zip(arguments) {
                    new_environment.borrow_mut().set(name, value);
                };

                self.environment = new_environment;

                let mut return_value: Option<Value> = None;

                for statement in body {
                    match self.run_statement(statement) {
                        Err(EvaluatorResults::Return(value)) => {
                            return_value = Some(value);
                            break;
                        },
                        _ => (),
                    };
                }

                self.environment = old_environment;

                if return_value.is_some() { return_value.unwrap() } else { Value::Nil }
            },
            _ => todo!(),
        })
    }

    pub fn path(&self) -> PathBuf {
        self.path.clone()
    }

    fn define_global_function(&mut self, name: impl Into<String>, callback: NativeFunctionCallback) {
        let name = name.into();

        self.globals.insert(name.clone(), Value::NativeFunction {
            name: name,
            callback: callback,
        });
    }

    fn env(&self) -> Ref<Environment> {
        RefCell::borrow(&self.environment)
    }

    fn env_mut(&mut self) -> RefMut<Environment> {
        RefCell::borrow_mut(&self.environment)
    }
    
    pub fn exec(&mut self, ast: Program) -> EvaluatorResult<()> {
        let mut ast = ast.into_iter();

        while let Some(statement) = ast.next() {
            self.run_statement(statement)?;
        }

        Ok(())
    }

    fn run(&mut self) -> EvaluatorResult<()> {
        while let Some(statement) = self.ast.next() {
            self.run_statement(statement.clone())?;
        }

        if ! ::std::env::args().filter(|a| a == "--debug").collect::<Vec<String>>().is_empty() {
            self.env().dump();
            dbg!(self.globals.clone());
        }

        Ok(())
    }
}
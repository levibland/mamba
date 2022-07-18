use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::rc::Rc;
use std::cell::RefCell;
use std::iter::Iterator;
use std::collections::HashMap;
use parser::{Block, Parameter, Expression};

use crate::evaluator::*;

pub type NativeFunctionCallback = fn (&mut Evaluator, Vec<Value>) -> Value;

#[derive(Debug, Clone)]
pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: impl Into<String>, value: Value) {
        self.values.insert(name.into(), value);
    }

    pub fn get(&self, name: impl Into<String>) -> Option<Value> {
        if let Some(value) = self.values.get(&name.into()) {
            Some(value.clone())
        } else {
            None
        }
    }

    pub fn drop(&mut self, name: impl Into<String>) {
        self.values.remove(&name.into());
    }

    pub fn dump(&self) {
        dbg!(self.values.clone());
    }
}

#[derive(Clone)]
pub enum Value {
    Integer(i64),
    String(String),
    Nil,
    Bool(bool),
    List(Rc<RefCell<Vec<Value>>>),
    Function {
        name: String,
        params: Vec<Parameter>,
        body: Block,
        environment: Option<Environment>,
        context: Option<Expression>,
    },
    NativeFunction {
        name: String,
        callback: NativeFunctionCallback,
    },
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", match self {
            Value::Integer(i) => i.to_string(),
            Value::String(s) => s.to_string(),
            Value::Nil => "nil".to_string(),
            Value::NativeFunction { name, .. } => format!("<{}>", name),
            Value::Function { name, params, .. } => format!("<{}>({})", name, params.into_iter().map(|p| p.name.clone()).collect::<Vec<String>>().join(", ")),
            Value::Bool(true) => "true".to_string(),
            Value::Bool(false) => "false".to_string(),
            Value::List(items) => {
                let mut buffer = String::from("[");
                let items = items.borrow();

                for (i, item) in items.iter().enumerate() {
                    buffer.push_str(&item.clone().to_string());

                    if i != items.len() - 1 {
                        buffer.push_str(", ");
                    }
                }

                buffer.push_str("]");

                buffer
            }
            _ => todo!(),
        })
    }
}

pub trait ToInt {
    fn to_int(self) -> i64;
}

pub trait ToVec {
    type Item;

    fn to_vec(self) -> Self::Item;
}

pub trait ToString {
    fn to_string(self) -> String;
}

pub trait ToBool {
    fn to_bool(self) -> bool;
}

impl ToInt for Value {
    fn to_int(self) -> i64 {
        match self {
            Value::Integer(i) => i,
            Value::Bool(true) => 1,
            Value::Nil | Value::Bool(false) => 0,
            Value::String(s) => {
                match s.trim().parse::<i64>() {
                    Ok(i) => i,
                    Err(_) => 0,
                }
            }
            _ => unreachable!(),
        }
    }
}

impl ToVec for Value {
    type Item = Rc<RefCell<Vec<Value>>>;

    fn to_vec(self) -> Self::Item {
        match self {
            Value::List(list) => list,
            _ => unreachable!(),
        }
    }
}

impl ToString for Value {
    fn to_string(self) -> String {
        match self {
            Value::String(s) => s,
            Value::Integer(i) => i.to_string(),
            Value::Bool(_) => self.to_int().to_string(),
            Value::Nil => "".to_string(),
            v @ Value::Function { .. } | v @ Value::List(..) => format!("{:?}", v),
            _ => unreachable!(),
        }
    }
}

impl ToBool for Value {
    fn to_bool(self) -> bool {
        match self {
            Value::Bool(true) | Value::Function { .. } => true,
            Value::String(s) => !s.is_empty(),
            Value::Integer(i) => i > 0,
            _ => false,
        }
    }
}

impl Value {
    pub fn is(self, other: Value) -> bool {
        match (self, other.clone()) {
            (Value::String(l), r) => l == r.to_string(),
            (Value::Integer(i), r) => i == r.to_int(),
            (Value::Bool(true), r) => r.to_bool() == true,
            (Value::Bool(false), r) => r.to_bool() == false,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }

    pub fn typestring(self) -> String {
        match self {
            Value::String(..) => "string".into(),
            Value::Integer(..) => "int".into(),
            Value::Bool(..) => "bool".into(),
            Value::Nil => "nil".into(),
            Value::Function { .. } | Value::NativeFunction { .. } => "function".into(),
            Value::List(..) => "list".into(),
            _ => unreachable!(),
        }
    }
}
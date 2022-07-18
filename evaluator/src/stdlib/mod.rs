use crate::evaluator::{Evaluator, EvaluatorResults};
use crate::environment::*;

pub fn arity(name: &str, arity: usize, arguments: &Vec<Value>) {
    if arity != arguments.len() {
        panic!("Method {} expected {} arguments, received {}.", name, arity, arguments.len());
    }
}

pub fn println(_: &mut Evaluator, args: Vec<Value>) -> Value {
    let arg = args.get(0).unwrap().clone();

    println!("{}", arg.to_string());

    Value::Nil
}

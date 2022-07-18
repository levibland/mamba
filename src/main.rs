#[macro_use] extern crate clap;

use ::evaluator::*;
use ::lexer::*;
use ::parser::*;
use std::fs::File;
use std::io::prelude::*;

mod commands;
use crate::commands::*;

fn read_file(file_path: String) -> Result<String, ::std::io::Error> {
    let mut file = File::open(file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn main() {
    let file_path = match commands::read_command() {
        Command::FileRead(file_path) => Some(file_path),
        Command::Noop => None,
    }.unwrap();

    let path = std::path::PathBuf::from(file_path.clone());
    let contents = read_file(file_path).unwrap();
    let tokens = Lex::lex(contents.as_str());
    let mut parser = Parser::new(tokens.iter());
    
    match parser.parse() {
        Ok(ast) => {
            match evaluate(ast, path) {
                Ok(_) => {},
                Err(e) => e.print(),
            };
        },
        Err(e) => e.print(),
    }
}

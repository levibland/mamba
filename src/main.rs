#[macro_use] extern crate clap;

use mamba_evaluator::*;
use mamba_lexer::*;
use mamba_parser::*;
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
    let code_string = match commands::read_command() {
        Command::FileRead(file_path) => read_file(file_path).ok(),
        Command::RunInlineCode(code) => Some(code),
        Command::Noop => None,
    };

    if let Some(code_string) = code_string {
        let lexer = Lexer::new(code_string);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let mut evaluator = Evaluator::new();
        let eval = evaluator.eval_program(program);
        println!("{}", eval);
    }
}

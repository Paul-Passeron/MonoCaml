use std::{env, fs};

use crate::typechecker::{Context, TypeChecker};

pub mod ast;
pub mod common;
pub mod parser;
pub mod typechecker;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];

    let src = fs::read_to_string(filename).expect("Failed to read file");
    match parser::parse(&src) {
        Ok(ast) => {
            println!("AST from file: {}", filename);
            println!("{:#?}", ast);
            println!(
                "TYPE: {:#?}",
                TypeChecker::type_of(&ast[0], &mut Context::new())
            );
        }
        Err(err) => {
            eprintln!("Parse error:\n{}", err);
            std::process::exit(1);
        }
    }
}

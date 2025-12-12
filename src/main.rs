#![feature(normalize_lexically)]
// #![allow(unused)]

use std::{env, fs};

use crate::typechecker::{Context, TypeChecker};
pub mod ast;
pub mod common;
pub mod lexing;
pub mod parser;
pub mod parsing;
pub mod platform;
pub mod typechecker;
pub mod utils;

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

            let mut ctx = Context::new();

            for expr in ast {
                println!("TYPE: {}", TypeChecker::type_of(&expr, &mut ctx).unwrap());
            }
        }
        Err(err) => {
            eprintln!("Parse error:\n{}", err);
            std::process::exit(1);
        }
    }
}

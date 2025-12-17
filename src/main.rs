#![feature(normalize_lexically)]
// #![allow(unused)]

use std::{env, fs};

use crate::{
    // parsing::parser::test_example,
    parsing::parser::MonoCamlParser,
};
pub mod ast;
pub mod common;
pub mod lexing;
pub mod parsing;
pub mod platform;
pub mod typechecker;
pub mod utils;

#[cfg(test)]
pub mod test;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];

    let src = fs::read_to_string(filename).expect("Failed to read file");

    let p = MonoCamlParser::new(filename, &src);
    let parse_tree = p.parse_ocaml_source().unwrap();
    for p in parse_tree {
        println!("{}", p);
    }
    // let _ = dbg!(parse_tree);
}

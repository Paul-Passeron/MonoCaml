#![feature(normalize_lexically, exit_status_error)]

use std::{fs::File, io::Read, path::PathBuf, sync::Mutex};

use lazy_static::lazy_static;

use crate::{
    backend::{Backend, llvm_backend::LLVMBackend},
    inference::{InferenceCtx, InferenceResult},
    lexer::{
        Lexer,
        error::LexingError,
        interner::{StrLit, Symbol},
    },
    lower::mono_to_cfg::MonoToCfg,
    monomorph::MonoCtx,
    parser::Parser,
    resolution::Resolver,
    session::Session,
    source_manager::SourceManager,
};

pub mod backend;
pub mod cfg;
pub mod examples;
pub mod helpers;
pub mod inference;
pub mod lexer;
pub mod lower;
pub mod mono_ir;
pub mod monomorph;
pub mod parse_tree;
pub mod parser;
pub mod poly_ir;
pub mod resolution;
pub mod session;
pub mod source_manager;

lazy_static! {
    static ref SESSION: Mutex<Session> = Mutex::new(Session::new(SourceManager::new()));
}

fn resolve_symbol(symbol: Symbol) -> String {
    SESSION.lock().unwrap().resolve_symbol(symbol).to_string()
}

fn resolve_strlit(strlit: StrLit) -> String {
    SESSION.lock().unwrap().resolve_strlit(strlit).to_string()
}

fn intern_symbol(s: &str) -> Symbol {
    SESSION.lock().unwrap().intern_symbol(s)
}

fn fresh_symbol() -> Symbol {
    SESSION.lock().unwrap().fresh_symbol()
}

#[allow(unused)]
fn intern_strlit(s: &str) -> StrLit {
    SESSION.lock().unwrap().intern_strlit(s)
}

fn main() {
    let file_name = "examples/hello_world.ml";
    let contents = {
        let mut s = String::new();
        let mut f = File::open(file_name).unwrap();
        f.read_to_string(&mut s).unwrap();
        s
    };
    let id = SESSION
        .lock()
        .unwrap()
        .source_manager
        .add_file(PathBuf::from(file_name), contents.to_string());
    let mut l = Lexer::new(id);
    let mut tokens = vec![];
    let mut error: Option<LexingError> = None;
    loop {
        let t = l.next_token();
        match t {
            Ok(x) => tokens.push(x),
            Err(LexingError::EOF) => {
                break;
            }
            Err(e) => {
                error = Some(e);
                break;
            }
        }
    }

    if let Some(error) = error {
        eprintln!("Lexing error: {}", error)
    }

    let mut parser = Parser::new(id, &tokens).unwrap();
    let prog = match parser.parse_program() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            return;
        }
    };

    let mut resolver = Resolver::new();

    let poly = resolver.resolve_structure(&prog).unwrap();

    let Resolver {
        types,
        mut vars,
        builtins,
        constructors,
        ..
    } = resolver;
    let InferenceResult {
        items: inferred,
        builtins,
    } = {
        let mut infer_ctx = InferenceCtx::new(&types, &vars, &builtins, &constructors);
        infer_ctx.infer_program(&poly).unwrap()
    };

    let mut mono_ctx = MonoCtx::new(&inferred, &mut vars, builtins, &types);
    let specialized = mono_ctx.mono_program().unwrap();
    let builtins = mono_ctx.get_final_builtins();
    let compiled = MonoToCfg::compile(&specialized, &builtins, &vars, &types);

    LLVMBackend::new(PathBuf::from("out.bin"))
        .compile(&compiled)
        .unwrap();
}

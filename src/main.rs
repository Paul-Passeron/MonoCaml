#![feature(normalize_lexically, exit_status_error)]

use std::{
    fs::File,
    io::Read,
    path::PathBuf,
    process::{Command, Stdio},
    sync::Mutex,
};

use lazy_static::lazy_static;

use crate::{
    backend::llvm_backend::LLVMBackend,
    cfg::{FunName, Label, var::CfgVar},
    lexer::{
        Lexer,
        error::LexingError,
        interner::{StrLit, Symbol},
    },
    lower::mono_to_cfg::MonoToCfg,
    mono_ir::{Ast, Var, types::AstCtx},
    parser::Parser,
    resolution::Resolver,
    session::Session,
    source_manager::SourceManager,
};

pub mod backend;
pub mod cfg;
pub mod examples;
pub mod helpers;
pub mod lexer;
pub mod lower;
pub mod mono_ir;
pub mod parse_tree;
pub mod parser;
pub mod poly_ir;
pub mod resolution;
pub mod session;
pub mod source_manager;

#[allow(unused)]
fn compile_ast<S: ToString>(ast: Ast<()>, prog_name: S) {
    compile_ast_with_ctx(ast, prog_name, AstCtx::default())
}

fn compile_ast_with_ctx<S: ToString>(ast: Ast<()>, prog_name: S, ctx: AstCtx) {
    let prog_name = prog_name.to_string();
    println!("{}", ast.display());
    let prog = MonoToCfg::compile(ast, ctx);
    println!("{prog}");

    let _ = prog
        .compile(LLVMBackend::new(&PathBuf::from(format!("./{prog_name}"))))
        .unwrap();

    Var::reset();
    CfgVar::reset();
    Label::reset();
    FunName::reset();
}

fn run_and_check_output(program: &str, args: &[&str], expected: &str) -> std::io::Result<bool> {
    let mut child = Command::new(program)
        .args(args)
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()?;

    let mut stdout = String::new();
    child.stdout.take().unwrap().read_to_string(&mut stdout)?;
    child.wait()?;

    Ok(stdout.contains(expected))
}

lazy_static! {
    static ref SESSION: Mutex<Session> = Mutex::new(Session::new(SourceManager::new()));
}

fn resolve_symbol(symbol: Symbol) -> String {
    SESSION.lock().unwrap().resolve_symbol(symbol).to_string()
}

fn resolve_strlit(strlit: StrLit) -> String {
    SESSION.lock().unwrap().resolve_strlit(strlit).to_string()
}

fn main() {
    let file_name = "examples/type_empty_cons.ml";
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

    for t in &tokens {
        println!("{t}");
    }

    if error.is_some() {
        println!("Lexing error: {}", error.unwrap())
    }

    let mut parser = Parser::new(id, &tokens).unwrap();
    let prog = match parser.parse_program() {
        Ok(p) => p,
        Err(e) => {
            println!("Parsing error: {:?}", e);
            return;
        }
    };

    let poly = Resolver::new().resolve_structure(&prog).unwrap();
    println!("{poly:#?}")
}

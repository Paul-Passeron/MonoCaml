#![feature(normalize_lexically, exit_status_error)]
#![feature(random)]

use std::{
    fs::File,
    io::Read,
    path::PathBuf,
    process::{Command, Stdio},
};

use crate::{
    backend::llvm_backend::LLVMBackend,
    cfg::{FunName, Label, var::CfgVar},
    lexer::{Lexer, LexingError},
    lower::mono_to_cfg::MonoToCfg,
    mono_ir::{Ast, Var, types::AstCtx},
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

fn main() {
    // let (ast, ctx) = rev_bench(100, 100);
    // compile_ast_with_ctx(ast, "llvm_test", ctx);
    let sm = SourceManager::new();
    let mut session = Session::new(sm);
    let contents = {
        let mut s = String::new();
        let mut f = File::open("examples/hello_world.ml").unwrap();
        f.read_to_string(&mut s).unwrap();
        s
    };
    let id = session.source_manager.add_file(
        PathBuf::from("examples/hello_world.ml"),
        contents.to_string(),
    );
    let mut l = Lexer::new(&session.source_manager, id);
    let mut tokens = vec![];
    let mut error: Option<LexingError> = None;
    loop {
        let t = l.next_token(&mut session);
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

    for t in tokens {
        println!("{}", t.display(&session));
    }

    if error.is_some() {
        println!("Lexing error: {}", error.unwrap().display(&session))
    }
}

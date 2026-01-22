#![feature(exit_status_error)]
#![feature(random)]

use std::{
    io::Read,
    path::PathBuf,
    process::{Command, Stdio},
};

use crate::{
    backend::llvm_backend::LLVMBackend,
    cfg::{FunName, Label, var::CfgVar},
    examples::rev_bench,
    lower::mono_to_cfg::MonoToCfg,
    mono_ir::{Ast, Var, types::AstCtx},
};

pub mod backend;
pub mod cfg;
pub mod examples;
pub mod helpers;
pub mod lexer;
pub mod lower;
pub mod mono_ir;
pub mod session;

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
    let (ast, ctx) = rev_bench(100, 100);
    compile_ast_with_ctx(ast, "llvm_test", ctx);
}

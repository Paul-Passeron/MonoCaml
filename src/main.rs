use std::{fs::File, path::PathBuf, process};

use crate::{
    ast::{Ast, AstTy, AstTyped, Var},
    cfg::compile::Compiler,
};

pub mod ast;
pub mod cfg;
pub mod helpers;

fn test_ast() -> Ast {
    let x = Var::fresh();
    let y = Var::fresh();
    Ast::lambda(
        AstTyped::new(x, AstTy::int()),
        Ast::lambda(
            AstTyped::new(y, AstTy::int()),
            Ast::app(
                Ast::app(
                    Ast::native("add"),
                    Ast::seq(Ast::app(Ast::native("print_int"), Ast::var(x)), Ast::var(x)),
                ),
                Ast::var(y),
            ),
        ),
    )
}

fn test_ast2() -> Ast {
    let x = Var::fresh();
    let y = Var::fresh();
    Ast::app(
        Ast::app(
            Ast::lambda(
                AstTyped::new(x, AstTy::int()),
                Ast::lambda(
                    AstTyped::new(y, AstTy::int()),
                    Ast::app(
                        Ast::app(
                            Ast::native("add"),
                            Ast::seq(Ast::app(Ast::native("print_int"), Ast::var(x)), Ast::var(x)),
                        ),
                        Ast::var(y),
                    ),
                ),
            ),
            Ast::Int(5),
        ),
        Ast::Int(6),
    )
}

fn test_ast_3() -> Ast {
    let x = Var::fresh();
    let y = Var::fresh();
    let z = Var::fresh();
    let wrapper = Ast::lambda(
        AstTyped::new(x, AstTy::Int),
        Ast::lambda(
            AstTyped::new(y, AstTy::Int),
            Ast::lambda(
                AstTyped::new(z, AstTy::fun(AstTy::Int, AstTy::Int)),
                Ast::app(
                    Ast::app(
                        Ast::Native("add".into()),
                        Ast::app(Ast::Var(z), Ast::Var(x)),
                    ),
                    Ast::app(Ast::Var(z), Ast::Var(y)),
                ),
            ),
        ),
    );

    let a = Var::fresh();

    let print_ret = Ast::lambda(
        AstTyped::new(a, AstTy::Int),
        Ast::seq(Ast::app(Ast::native("print_int"), Ast::Var(a)), Ast::Var(a)),
    );

    Ast::app(
        Ast::app(Ast::app(wrapper, Ast::Int(69)), Ast::Int(420)),
        print_ret,
    )
}

fn main() {
    let ast = test_ast_3();
    println!("{ast}\n");
    let free_vars = ast.free_vars();
    for var in free_vars {
        println!("Free var {var}");
    }
    let prog = Compiler::compile(ast);
    prog.export_to_c(&mut File::create(PathBuf::from("./file.c")).unwrap());
    let out = process::Command::new("gcc")
        .arg("-o")
        .arg("test")
        .arg("file.c")
        .arg("runtime.c")
        .output()
        .unwrap();
    print!(
        "{}",
        out.stdout
            .into_iter()
            .map(|x| char::from_u32(x as u32).unwrap())
            .collect::<String>()
    );
    let out = process::Command::new("./test").output().unwrap();
    print!(
        "{}",
        out.stdout
            .into_iter()
            .map(|x| char::from_u32(x as u32).unwrap())
            .collect::<String>()
    );
}

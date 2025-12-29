use std::{
    fs::File,
    path::PathBuf,
    process::{self},
};

use crate::{
    ast::{Ast, AstTy, AstTyped, Var},
    cfg::compile::Compiler,
};

pub mod ast;
pub mod cfg;
pub mod helpers;

#[allow(unused)]
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

#[allow(unused)]
fn test_ast_2() -> Ast {
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

#[allow(unused)]
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

#[allow(unused)]
fn test_ast_4() -> Ast {
    Ast::app(Ast::native("print_int"), test_ast_3())
}

#[allow(unused)]
fn test_ast_5() -> Ast {
    let x = Var::fresh();

    Ast::let_binding(
        x,
        AstTy::Int,
        Ast::Int(5),
        Ast::app(
            Ast::native("print_int"),
            Ast::app(Ast::app(Ast::native("add"), Ast::Int(64)), Ast::Var(x)),
        ),
    )
}

#[allow(unused)]
fn test_ast_6() -> Ast {
    let x = Var::fresh();

    Ast::let_binding(
        x,
        AstTy::fun(AstTy::Int, AstTy::Tuple(vec![])),
        Ast::native("print_int"),
        Ast::app(Ast::Var(x), Ast::Int(420)),
    )
}

#[allow(unused)]
fn test_ast_7() -> Ast {
    Ast::app(Ast::native("print_string"), Ast::string("Hello, World !\n"))
}

#[allow(unused)]
fn test_ast_8() -> Ast {
    let x = Var::fresh();

    Ast::let_rec(
        x,
        AstTy::fun(AstTy::Int, AstTy::Tuple(vec![])),
        Ast::native("print_int"),
        Ast::app(Ast::Var(x), Ast::Int(420)),
    )
}

#[allow(unused)]
fn test_ast_9() -> Ast {
    let x = Var::fresh();
    let print_is_zero = Ast::lambda(
        AstTyped::new(x, AstTy::Int),
        Ast::app(
            Ast::native("print_string"),
            Ast::seq(
                Ast::app(Ast::native("print_int"), Ast::Var(x)),
                Ast::ifte(
                    Ast::var(x),
                    Ast::string(" is not zero\n"),
                    Ast::string(" is zero\n"),
                ),
            ),
        ),
    );
    let f = Var::fresh();
    Ast::let_binding(
        f,
        AstTy::fun(AstTy::Int, AstTy::Tuple(vec![])),
        print_is_zero,
        Ast::seq(
            Ast::app(Ast::Var(f), Ast::Int(420)),
            Ast::app(Ast::Var(f), Ast::Int(0)),
        ),
    )
}

fn compile_ast(ast: Ast) {
    println!("{ast}");
    let free_vars = ast.free_vars();
    for var in free_vars {
        println!("Free var {var}");
    }
    let prog = Compiler::compile(ast);
    // println!("{prog}\n");
    prog.export_to_c(&mut File::create(PathBuf::from("./file.c")).unwrap());
    process::Command::new("clang")
        .arg("-g")
        .arg("-o")
        .arg("test")
        .arg("file.c")
        .arg("runtime.c")
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    process::Command::new("./test")
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
}

fn main() {
    compile_ast(test_ast_9());
}

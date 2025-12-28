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
                    Ast::seq(Ast::app(Ast::native("print"), Ast::var(x)), Ast::var(x)),
                ),
                Ast::var(y),
            ),
        ),
    )
}

fn main() {
    let ast = test_ast();
    println!("{ast}\n");
    let free_vars = ast.free_vars();
    for var in free_vars {
        println!("Free var {var}");
    }
    let prog = Compiler::compile(ast);
    println!("{prog}\n");
}

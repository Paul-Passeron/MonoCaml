use crate::ast::{Ast, Ty, Typed, Var};

pub mod ast;
pub mod helpers;

fn test_ast() -> Ast {
    let x = Var::fresh();
    let y = Var::fresh();
    Ast::lambda(
        Typed::new(x, Ty::int()),
        Ast::lambda(
            Typed::new(y, Ty::int()),
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
    println!("{}", test_ast());
    Var::reset();
    println!("{}", test_ast());
}

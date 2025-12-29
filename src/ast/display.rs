use std::fmt;

use crate::ast::{Ast, AstTy, AstTyped, Var};

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.extract())
    }
}

impl fmt::Display for AstTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstTy::Int => write!(f, "int"),
            AstTy::String => write!(f, "string"),
            AstTy::Tuple(items) => write!(
                f,
                "({})",
                items
                    .iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            AstTy::Fun { arg, ret } => write!(f, "({} -> {})", arg, ret),
        }
    }
}

impl<T: fmt::Display> fmt::Display for AstTyped<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}:{})", self.expr, self.ty)
    }
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ast::Str(s) => write!(f, "\"{}\"", &s[..].escape_debug()),
            Ast::Int(i) => write!(f, "{}", i),
            Ast::Var(var) => write!(f, "{}", var),
            Ast::Lambda { arg, body } => write!(f, "(λ{}.{})", arg, body),
            Ast::App { fun, arg } => write!(f, "({} {})", fun, arg),
            Ast::Seq { fst, snd } => write!(f, "({}; {})", fst, snd),
            Ast::Tuple(asts) => write!(
                f,
                "({})",
                asts.iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Ast::Get { from, index } => write!(f, "({}.{})", from, index),
            Ast::Native(name) => write!(f, "{name}"),
            Ast::LetBinding { .. } => todo!(),
        }
    }
}

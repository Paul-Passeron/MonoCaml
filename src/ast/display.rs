use std::fmt;

use crate::ast::{Ast, Ty, Typed, Var};

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.extract())
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Int => write!(f, "int"),
            Ty::String => write!(f, "string"),
            Ty::Tuple(items) => write!(
                f,
                "({})",
                items
                    .iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Ty::Fun { arg, ret } => write!(f, "({} -> {})", arg, ret),
        }
    }
}

impl<T: fmt::Display> fmt::Display for Typed<T> {
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
        }
    }
}

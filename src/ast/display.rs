use std::fmt;

use crate::ast::{
    Ast, AstTy, AstTyped, Var,
    types::{EnumCase, EnumDef},
};

pub fn alphabetize(x: usize) -> String {
    let mut x = x + 1;
    let chars = "abcdefghijklmnopqrstuvwxyz".chars().collect::<Vec<_>>();
    let mut result = String::new();
    while x > 0 {
        let index = (x - 1) % chars.len();
        result.push(chars[index]);
        x = (x - 1) / chars.len();
    }
    result.chars().rev().collect()
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", alphabetize(self.extract()))
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
            AstTy::Named(s) => write!(f, "{s}"),
        }
    }
}

impl<T: fmt::Display> fmt::Display for AstTyped<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}:{})", self.expr(), self.ty())
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
            Ast::LetBinding {
                bound,
                value,
                in_expr,
            } => write!(f, "let {} = {} in {}", bound, value, in_expr),
            Ast::If {
                cond,
                then_e,
                else_e,
            } => {
                write!(f, "if {} then {} else {}", cond, then_e, else_e)
            }
            Ast::Cons {
                enum_name,
                arg,
                case,
            } => write!(
                f,
                "({}.{}{})",
                enum_name,
                case,
                match arg {
                    Some(x) => format!(" {x}"),
                    None => "".into(),
                }
            ),
            Ast::Match { expr, cases } => {
                writeln!(f, "match {expr} with").unwrap();
                for case in cases {
                    writeln!(f, "| {} -> {}", case.pat, case.expr).unwrap()
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for EnumCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "| {}{}",
            self.cons_name,
            match &self.arg {
                None => "".into(),
                Some(ty) => format!(" of {}", {
                    let t = ty.to_string();
                    let as_str = t.as_str();
                    as_str
                        .strip_prefix("(")
                        .map(|x| x.strip_suffix(")").unwrap())
                        .unwrap_or(as_str)
                        .to_string()
                }),
            }
        )
    }
}

impl fmt::Display for EnumDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "enum {}", self.name)?;
        for case in &self.cases {
            writeln!(f, "    {case}")?;
        }
        Ok(())
    }
}

use std::{collections::HashSet, fmt};

use crate::ast::{Var, types::AstTy};

#[derive(Clone)]
pub enum Pattern {
    Int(i32),
    Symbol(Var, AstTy),
    Cons {
        enum_name: String,
        cons: String,
        arg: Option<Box<Pattern>>,
    },
    Tuple(Vec<Pattern>),
}

impl Pattern {
    pub fn int(x: i32) -> Self {
        Self::Int(x)
    }

    pub fn symb(x: Var, ty: AstTy) -> Self {
        Self::Symbol(x, ty)
    }

    pub fn cons<S1: ToString, S2: ToString>(enum_name: S1, cons: S2, arg: Option<Self>) -> Self {
        Self::Cons {
            enum_name: enum_name.to_string(),
            cons: cons.to_string(),
            arg: arg.map(Box::new),
        }
    }

    pub fn tuple(pats: Vec<Self>) -> Self {
        Self::Tuple(pats)
    }

    fn vars_aux(&self, s: &mut HashSet<Var>) {
        match self {
            Pattern::Int(_) => (),
            Pattern::Symbol(var, _) => {
                s.insert(var.clone());
            }
            Pattern::Cons { arg, .. } => arg.iter().for_each(|x| x.vars_aux(s)),
            Pattern::Tuple(patterns) => patterns.iter().for_each(|x| x.vars_aux(s)),
        }
    }

    pub fn vars(&self) -> HashSet<Var> {
        let mut s = HashSet::new();
        self.vars_aux(&mut s);
        s
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Int(x) => write!(f, "{x}"),
            Pattern::Symbol(var, _) => write!(f, "{var}"),
            Pattern::Cons {
                enum_name,
                cons,
                arg,
            } => write!(
                f,
                "{}.{}{}",
                enum_name,
                cons,
                match arg {
                    Some(x) => format!(" {x}"),
                    None => "".into(),
                }
            ),
            Pattern::Tuple(patterns) => write!(
                f,
                "({})",
                patterns
                    .iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

use std::fmt;

use crate::{
    lexer::interner::Symbol,
    parse_tree::{ArgLabel, Located, LongIdent},
    resolve_symbol,
};

pub type TypeExpr = Located<TypeExprDesc>;

#[derive(Debug)]
pub enum TypeExprDesc {
    Any,                                           // _
    Var(Symbol),                                   // 'a
    Arrow(ArgLabel, Box<TypeExpr>, Box<TypeExpr>), // t1 -> t2
    Tuple(Vec<TypeExpr>),                          // t1 * t2 * ...
    Constr(Vec<TypeExpr>, LongIdent),              // t, int list, (int, string) Hashtbl.t
    Alias(Box<TypeExpr>, Symbol),                  // t as 'a

                                                   // TODO: other type expressions: Which one do we need / want to support ?
}

const INDENT: &str = "    ";

pub struct TypeExprDescDisplay<'a> {
    pub desc: &'a TypeExprDesc,
    pub indent: usize,
}

impl TypeExprDesc {
    pub fn display<'a>(&'a self, indent: usize) -> TypeExprDescDisplay<'a> {
        TypeExprDescDisplay { desc: self, indent }
    }
}

impl fmt::Display for TypeExprDescDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", INDENT.repeat(self.indent))?;
        match &self.desc {
            TypeExprDesc::Any => write!(f, "_"),
            TypeExprDesc::Var(symbol) => {
                write!(f, "'{}", resolve_symbol(*symbol))
            }
            TypeExprDesc::Arrow(label, t1, t2) => {
                match label {
                    ArgLabel::NoLabel => {}
                    ArgLabel::Labelled(sym, _) => {
                        write!(f, "~{}: ", resolve_symbol(*sym))?;
                    }
                    ArgLabel::Optional(sym, _) => {
                        write!(f, "?{}: ", resolve_symbol(*sym))?;
                    }
                }
                write!(f, "{} -> {}", t1.desc.display(0), t2.desc.display(0))
            }
            TypeExprDesc::Tuple(types) => {
                for (i, typ) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, " * ")?;
                    }
                    write!(f, "{}", typ.desc.display(0))?;
                }
                Ok(())
            }
            TypeExprDesc::Constr(params, long_ident) => {
                if params.is_empty() {
                    write!(f, "{}", long_ident.display(0))
                } else if params.len() == 1 {
                    write!(f, "{} {}", params[0].desc.display(0), long_ident.display(0))
                } else {
                    write!(f, "(")?;
                    for (i, param) in params.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", param.desc.display(0))?;
                    }
                    write!(f, ") {}", long_ident.display(0))
                }
            }
            TypeExprDesc::Alias(typ, symbol) => {
                write!(f, "{} as '{}", typ.desc.display(0), resolve_symbol(*symbol))
            }
        }
    }
}

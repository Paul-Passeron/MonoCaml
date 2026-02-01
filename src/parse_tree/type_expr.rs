use std::fmt;

use crate::{
    lexer::interner::Symbol,
    parse_tree::{ArgLabel, Located, LongIdent},
    session::Session,
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

const INDENT: &'static str = "    ";

pub struct TypeExprDescDisplay<'a, 'b> {
    pub desc: &'a TypeExprDesc,
    pub session: &'b Session,
    pub indent: usize,
}

impl TypeExprDesc {
    pub fn display<'a, 'b>(
        &'a self,
        session: &'b Session,
        indent: usize,
    ) -> TypeExprDescDisplay<'a, 'b> {
        TypeExprDescDisplay {
            desc: self,
            session,
            indent,
        }
    }
}

impl fmt::Display for TypeExprDescDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", INDENT.repeat(self.indent))?;
        match &self.desc {
            TypeExprDesc::Any => write!(f, "_"),
            TypeExprDesc::Var(symbol) => {
                write!(f, "'{}", symbol.display(&self.session.symbol_interner))
            }
            TypeExprDesc::Arrow(label, t1, t2) => {
                match label {
                    ArgLabel::NoLabel => {}
                    ArgLabel::Labelled(sym, _) => {
                        write!(f, "~{}: ", sym.display(&self.session.symbol_interner))?;
                    }
                    ArgLabel::Optional(sym, _) => {
                        write!(f, "?{}: ", sym.display(&self.session.symbol_interner))?;
                    }
                }
                write!(
                    f,
                    "{} -> {}",
                    t1.desc.display(self.session, 0),
                    t2.desc.display(self.session, 0)
                )
            }
            TypeExprDesc::Tuple(types) => {
                for (i, typ) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, " * ")?;
                    }
                    write!(f, "{}", typ.desc.display(self.session, 0))?;
                }
                Ok(())
            }
            TypeExprDesc::Constr(params, long_ident) => {
                if params.is_empty() {
                    write!(f, "{}", long_ident.display(self.session, 0))
                } else if params.len() == 1 {
                    write!(
                        f,
                        "{} {}",
                        params[0].desc.display(self.session, 0),
                        long_ident.display(self.session, 0)
                    )
                } else {
                    write!(f, "(")?;
                    for (i, param) in params.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", param.desc.display(self.session, 0))?;
                    }
                    write!(f, ") {}", long_ident.display(self.session, 0))
                }
            }
            TypeExprDesc::Alias(typ, symbol) => {
                write!(
                    f,
                    "{} as '{}",
                    typ.desc.display(self.session, 0),
                    symbol.display(&self.session.symbol_interner)
                )
            }
        }
    }
}

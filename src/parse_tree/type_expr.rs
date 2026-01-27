use crate::{
    lexer::interner::Symbol,
    parse_tree::{Located, LongIdent},
};

pub type TypeExpr = Located<TypeExprDesc>;

pub enum TypeExprDesc {
    Any,                                           // _
    Var(Symbol),                                   // 'a
    Arrow(ArgLabel, Box<TypeExpr>, Box<TypeExpr>), // t1 -> t2
    Tuple(Vec<TypeExpr>),                          // t1 * t2 * ...
    Constr(Vec<TypeExpr>, LongIdent),              // t, int list, (int, string) Hashtbl.t
    Alias(Box<TypeExpr>, Symbol),                  // t as 'a

                                                   // TODO: other type expressions: Which one do we need / want to support ?
}

pub enum ArgLabel {
    NoLabel,          // f x
    Labelled(Symbol), // f ~x
    Optional(Symbol), // f ?x
}

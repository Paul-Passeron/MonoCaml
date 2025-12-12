use crate::{
    lexing::Position,
    parsing::{
        ast_helper::StrOpt,
        asttypes::RecFlag,
        docstring::{Docs, Text},
        location::Location,
        parsetree::{
            Attributes, CoreType, CoreTypeDesc, Expression, Pattern, PatternDesc, ValueConstraint,
        },
    },
};

pub fn mktyp(loc: (Position, Position), attrs: Option<Attributes>, d: CoreTypeDesc) -> CoreType {
    CoreType::mk(Some(Location::make(loc.0, loc.1)), attrs, d)
}

pub fn mkpat(loc: (Position, Position), attrs: Option<Attributes>, d: PatternDesc) -> Pattern {
    Pattern::mk(Some(Location::make(loc.0, loc.1)), attrs, d)
}

pub struct LetBinding {
    pattern: Pattern,
    expr: Expression,
    constraint: Option<ValueConstraint>,
    is_pun: bool,
    attributes: Attributes,
    docs: Docs,
    text: Text,
    loc: Location,
}

pub struct LetBindings {
    bindings: Vec<LetBinding>,
    rec: RecFlag,
    extension: StrOpt,
}

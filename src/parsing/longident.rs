use crate::parsing::asttypes::Loc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LongIdent {
    Ident(String),
    Dot(Loc<Box<LongIdent>>, Loc<String>),
    Apply(Loc<Box<LongIdent>>, Loc<Box<LongIdent>>),
}

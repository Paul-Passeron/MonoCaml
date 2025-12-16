use crate::parsing::{asttypes::Loc, location::Location};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LongIdent {
    Ident(String),
    Dot(Loc<Box<LongIdent>>, Loc<String>),
    Apply(Loc<Box<LongIdent>>, Loc<Box<LongIdent>>),
}

impl LongIdent {
    pub fn ident<S>(str: S, l: Location) -> Loc<Self>
    where
        S: Into<String>,
    {
        Loc::new(LongIdent::Ident(str.into()), l)
    }
}

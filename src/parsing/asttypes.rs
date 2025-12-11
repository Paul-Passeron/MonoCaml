use crate::{parsing::location::Location, platform::NativeInt};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constant<Nat: NativeInt> {
    Int(i64),
    Char(u8),
    String(String, Location, Option<String>),
    Float(String),
    Int32(i32),
    Int64(i64),
    NativeInt(Nat),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RecFlag {
    Nonrecursive,
    Recursive,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DirectionFlag {
    Upto,
    Downto,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrivateFlag {
    Private,
    Public,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MutableFlag {
    Immutable,
    Mutable,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AtomicFlag {
    Nonatomic,
    Atomic,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VirtualFlag {
    Virtual,
    Concrete,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OverrideFlag {
    Override,
    Fresh,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ClosedFlag {
    Closed,
    Open,
}

pub type Label = String;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArgLabel {
    NoLabel,
    Labelled(String),
    Optional(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loc<T> {
    pub txt: T,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Variance {
    Covariant,
    Contravariant,
    NoVariance,
    Bivariant,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Injectivity {
    Injective,
    NoInjectivity,
}

impl ToString for ArgLabel {
    fn to_string(&self) -> String {
        match self {
            ArgLabel::NoLabel => String::new(),
            ArgLabel::Labelled(s) => format!("{s}"),
            ArgLabel::Optional(s) => format!("?{s}"),
        }
    }
}

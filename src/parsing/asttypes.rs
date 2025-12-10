use crate::{parsing::location::Location, platform::NativeInt};

pub enum Constant<Nat: NativeInt> {
    Int(i64),
    Char(u8),
    String(String, Location, Option<String>),
    Float(String),
    Int32(i32),
    Int64(i64),
    NativeInt(Nat),
}

pub enum RecFlag {
    Nonrecursive,
    Recursive,
}

pub enum DirectionFlag {
    Upto,
    Downto,
}

pub enum PrivateFlag {
    Private,
    Public,
}

pub enum MutableFlag {
    Immutable,
    Mutable,
}

pub enum AtomicFlag {
    Nonatomic,
    Atomic,
}

pub enum VirtualFlag {
    Virtual,
    Concrete,
}

pub enum OverrideFlag {
    Override,
    Fresh,
}

pub enum ClosedFlag {
    Closed,
    Open,
}

pub type Label = String;

pub enum ArgLabel {
    NoLabel,
    Labelled(String),
    Optional(String),
}

pub struct Loc<T> {
    pub txt: T,
    pub loc: Location,
}

pub enum Variance {
    Covariant,
    Contravariant,
    NoVariance,
    Bivariant,
}

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

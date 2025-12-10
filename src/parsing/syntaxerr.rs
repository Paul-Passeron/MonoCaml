use crate::parsing::location::Location;

pub enum InvalidPackageType {
    ParametrizedTypes,
    ConstrainedTypes,
    PrivateTypes,
    NotWithType,
    NeitherIdentifierNorWithType,
}

pub enum Error {
    Unclosed(Location, String, Location, String),
    Expecting(Location, String),
    NotExpecting(Location, String),
    ApplicativePath(Location),
    VariableInScope(Location, String),
    Other(Location),
    IllFormedAst(Location, String),
    InvalidPackageType(Location, InvalidPackageType),
    RemovedStringSet(Location),
}

impl Error {
    pub fn location(&self) -> &Location {
        match self {
            Self::Unclosed(l, _, _, _)
            | Self::ApplicativePath(l)
            | Self::VariableInScope(l, _)
            | Self::Other(l)
            | Self::NotExpecting(l, _)
            | Self::IllFormedAst(l, _)
            | Self::InvalidPackageType(l, _)
            | Self::Expecting(l, _)
            | Self::RemovedStringSet(l) => l,
        }
    }

    pub fn ill_formed_ast<T>(loc: Location, s: String) -> Result<T, Self> {
        Err(Self::IllFormedAst(loc, s))
    }
}

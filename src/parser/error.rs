use crate::source_manager::loc::LocKind;

pub enum ParsingErrorKind {}

pub struct ParsingError {
    pub loc: LocKind,
    pub kind: ParsingErrorKind,
}

pub type ParseRes<T> = Result<T, ParsingError>;

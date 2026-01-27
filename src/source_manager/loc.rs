use crate::source_manager::FileId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Loc {
    pub file: FileId,
    pub offset: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub file: FileId,
    pub start: usize,
    pub end: usize,
}

impl Loc {
    pub fn new(file: FileId, offset: usize) -> Self {
        Loc { file, offset }
    }

    pub fn span(&self, other: &Self) -> Span {
        Span {
            file: self.file,
            start: self.offset,
            end: other.offset,
        }
    }
}

use std::fmt;

use crate::{SESSION, source_manager::FileId};

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

    pub fn previous(&self) -> Self {
        Self {
            file: self.file,
            offset: if self.offset == 0 { 0 } else { self.offset - 1 },
        }
    }
}

impl Span {
    pub fn split(&self) -> (Loc, Loc) {
        let l1 = Loc::new(self.file, self.start);
        let l2 = Loc::new(self.file, self.end);
        (l1, l2)
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sesh = SESSION.lock().unwrap();
        let loc_infos = sesh.source_manager.loc_infos(*self);
        write!(f, "{}:{}:{}", loc_infos.path, loc_infos.line, loc_infos.col)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sesh = SESSION.lock().unwrap();

        let (l1, l2) = self.split();
        let li1 = sesh.source_manager.loc_infos(l1);
        let li2 = sesh.source_manager.loc_infos(l2);
        write!(
            f,
            "{}:{}:{} {}:{}:{}",
            li1.path, li1.line, li1.col, li2.path, li2.line, li2.col
        )
    }
}

#[derive(Debug)]
pub enum LocKind {
    Span(Span),
    Loc(Loc),
}

// impl Into<LocKind> for Loc {
//     fn into(self) -> LocKind {
//         LocKind::Loc(self)
//     }
// }

// impl Into<LocKind> for Span {
//     fn into(self) -> LocKind {
//         LocKind::Span(self)
//     }
// }

impl From<Loc> for LocKind {
    fn from(value: Loc) -> Self {
        Self::Loc(value)
    }
}

impl From<Span> for LocKind {
    fn from(value: Span) -> Self {
        Self::Span(value)
    }
}

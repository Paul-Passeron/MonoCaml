use std::fmt;

use crate::source_manager::{FileId, SourceManager};

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

pub struct LocDisplay<'a>(Loc, &'a SourceManager);
pub struct SpanDisplay<'a>(Span, &'a SourceManager);

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

    pub fn display<'a>(&self, source_manager: &'a SourceManager) -> LocDisplay<'a> {
        LocDisplay(*self, source_manager)
    }
}

impl Span {
    pub fn split(&self) -> (Loc, Loc) {
        let l1 = Loc::new(self.file, self.start);
        let l2 = Loc::new(self.file, self.end);
        (l1, l2)
    }

    pub fn display<'a>(&self, source_manager: &'a SourceManager) -> SpanDisplay<'a> {
        SpanDisplay(*self, source_manager)
    }
}

impl<'a> fmt::Display for LocDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let loc_infos = self.1.loc_infos(self.0);
        write!(f, "{}:{}:{}", loc_infos.path, loc_infos.line, loc_infos.col)
    }
}

impl<'a> fmt::Display for SpanDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (l1, l2) = self.0.split();
        let li1 = self.1.loc_infos(l1);
        let li2 = self.1.loc_infos(l2);
        write!(
            f,
            "{}:{}:{} {}:{}:{}",
            li1.path, li1.line, li1.col, li2.path, li2.line, li2.col
        )
    }
}

pub enum LocKind {
    Span(Span),
    Loc(Loc),
}

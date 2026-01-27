use loc::Loc;
use std::path::{Path, PathBuf};

pub mod loc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct FileIdToken(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(FileIdToken);

pub struct FileInfo {
    pub kind: FileKind,
    pub contents: String,
    pub lines: Vec<usize>, // Positions of '\n' chars in the source
}

impl FileInfo {
    pub fn new(kind: FileKind, contents: String) -> Self {
        let lines = contents
            .chars()
            .into_iter()
            .enumerate()
            .filter_map(|(i, c)| if c == '\n' { Some(i) } else { None })
            .collect();

        Self {
            kind,
            contents,
            lines,
        }
    }
}

pub enum FileKind {
    StdIn,
    File(PathBuf),
}

pub struct SourceManager {
    files: Vec<FileInfo>,
}

impl FileKind {
    pub fn get_name(&self) -> String {
        match self {
            FileKind::StdIn => "stdin".into(),
            FileKind::File(path_buf) => path_buf.display().to_string(),
        }
    }
}

impl SourceManager {
    pub fn new() -> Self {
        SourceManager { files: Vec::new() }
    }

    fn fresh_id(&self) -> FileId {
        FileId(FileIdToken(self.files.len() as u32))
    }

    pub fn add_file<P: AsRef<Path>>(&mut self, path: P, contents: String) -> FileId {
        let p = path
            .as_ref()
            .normalize_lexically()
            .unwrap_or(path.as_ref().to_path_buf());

        let id = self.fresh_id();
        let file_info = FileInfo::new(FileKind::File(p), contents);
        self.files.push(file_info);
        id
    }

    pub fn add_stdin(&mut self, contents: String) -> FileId {
        let id = self.fresh_id();
        self.files.push(FileInfo::new(FileKind::StdIn, contents));
        id
    }

    pub fn get_file(&self, id: FileId) -> &FileInfo {
        &self.files[id.0.0 as usize]
    }

    pub fn loc_to_string(&self, loc: Loc) -> String {
        let f = self.get_file(loc.file);
        let (line, offset) = f
            .lines
            .iter()
            .copied()
            .enumerate()
            .find(|(_, off)| *off >= loc.offset)
            .unwrap_or_else(|| (f.lines.len(), loc.offset));
        let col = loc.offset - offset;

        format!("{}:{}:{}", f.kind.get_name(), line, col)
    }
}

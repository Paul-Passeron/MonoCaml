pub mod tokens;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Position {
    pub fname: String,
    pub lnum: u32,
    pub bol: usize,
    pub cnum: u32,
}

impl Position {
    pub const fn dummy() -> Self {
        Self {
            fname: String::new(),
            lnum: 0,
            bol: 0,
            cnum: 0,
        }
    }

    pub fn with_file_name<T: ToString>(self, name: T) -> Self {
        Self {
            fname: name.to_string(),
            lnum: self.lnum,
            bol: self.bol,
            cnum: self.cnum,
        }
    }
}

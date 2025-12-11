pub mod tokens;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Position {
    fname: String,
    lnum: u32,
    bol: usize,
    cnum: u32,
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

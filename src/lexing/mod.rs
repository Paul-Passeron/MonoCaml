pub struct Position {
    fname: String,
    lnum: u32,
    bol: usize,
    cnum: u32,
}

impl Position {
    pub fn dummy() -> Self {
        Self {
            fname: String::new(),
            lnum: 0,
            bol: 0,
            cnum: 0,
        }
    }
}

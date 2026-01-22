use std::{collections::HashMap, fmt};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StrLit(u32);

mod symbs {
    use super::{StrLit, Symbol};

    impl From<u32> for StrLit {
        fn from(value: u32) -> Self {
            Self(value)
        }
    }

    impl Into<u32> for StrLit {
        fn into(self) -> u32 {
            self.0
        }
    }

    impl From<u32> for Symbol {
        fn from(value: u32) -> Self {
            Self(value)
        }
    }

    impl Into<u32> for Symbol {
        fn into(self) -> u32 {
            self.0
        }
    }
}

pub struct Interner<'src, S>
where
    S: Copy + From<u32> + Into<u32>,
{
    map: HashMap<&'src str, S>,
    strings: Vec<&'src str>,
}

pub struct SymbolDisplayer<'a, 'src> {
    interner: &'a Interner<'src, Symbol>,
    symbol: Symbol,
}

impl<'a, 'src> fmt::Display for SymbolDisplayer<'a, 'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.interner.resolve(self.symbol))
    }
}

impl Symbol {
    pub fn display<'a, 'src>(
        &self,
        interner: &'a Interner<'src, Symbol>,
    ) -> SymbolDisplayer<'a, 'src> {
        SymbolDisplayer {
            interner,
            symbol: *self,
        }
    }
}

impl<'src, S> Interner<'src, S>
where
    S: Copy + From<u32> + Into<u32>,
{
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            strings: Vec::new(),
        }
    }

    pub fn intern(&mut self, s: &'src str) -> S {
        if let Some(symb) = self.map.get(s) {
            *symb
        } else {
            let idx = self.strings.len();
            self.strings.push(s);
            let symbol = S::from(idx as u32);
            self.map.insert(s, symbol);
            symbol
        }
    }

    pub fn resolve(&self, s: S) -> &'src str {
        self.strings[s.into() as usize]
    }
}

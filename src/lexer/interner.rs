use std::{collections::HashMap, fmt};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct SymbToken(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(SymbToken);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StrLit(SymbToken);

trait ExtractSymbToken {
    fn extract(&self) -> SymbToken;
    fn from(t: SymbToken) -> Self;
}

impl ExtractSymbToken for StrLit {
    fn extract(&self) -> SymbToken {
        self.0
    }

    fn from(t: SymbToken) -> Self {
        Self(t)
    }
}

impl ExtractSymbToken for Symbol {
    fn extract(&self) -> SymbToken {
        self.0
    }
    fn from(t: SymbToken) -> Self {
        Self(t)
    }
}

#[allow(private_bounds)]
pub struct Interner<S>
where
    S: Copy + ExtractSymbToken,
{
    map: HashMap<String, S>,
    strings: Vec<String>,
}

pub struct SymbolDisplayer<'a> {
    interner: &'a Interner<Symbol>,
    symbol: Symbol,
}

impl<'a> fmt::Display for SymbolDisplayer<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.interner.resolve(self.symbol))
    }
}

impl Symbol {
    pub fn display<'a>(&self, interner: &'a Interner<Symbol>) -> SymbolDisplayer<'a> {
        SymbolDisplayer {
            interner,
            symbol: *self,
        }
    }
}

#[allow(private_bounds)]
impl<S> Interner<S>
where
    S: Copy + ExtractSymbToken,
{
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            strings: Vec::new(),
        }
    }

    pub fn intern(&mut self, s: &str) -> S {
        if let Some(symb) = self.map.get(s) {
            *symb
        } else {
            let idx = self.strings.len();
            self.strings.push(s.into());
            let symbol = S::from(SymbToken(idx as u32));
            self.map.insert(s.into(), symbol);
            symbol
        }
    }

    pub fn resolve(&self, s: S) -> &String {
        &self.strings[s.extract().0 as usize]
    }

    pub fn lookup(&self, s: &str) -> Option<S> {
        self.map.get(s).copied()
    }
}

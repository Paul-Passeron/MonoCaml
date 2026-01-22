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
pub struct Interner<'src, S>
where
    S: Copy + ExtractSymbToken,
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

#[allow(private_bounds)]
impl<'src, S> Interner<'src, S>
where
    S: Copy + ExtractSymbToken,
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
            let symbol = S::from(SymbToken(idx as u32));
            self.map.insert(s, symbol);
            symbol
        }
    }

    pub fn resolve(&self, s: S) -> &'src str {
        self.strings[s.extract().0 as usize]
    }

    pub fn lookup(&self, s: &'src str) -> Option<S> {
        self.map.get(s).copied()
    }
}

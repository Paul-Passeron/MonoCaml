use std::{collections::HashMap, fmt};

use crate::SESSION;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct SymbToken(u32);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConsSymbol(SymbToken);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct IdentSymbol(SymbToken);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Symbol {
    Ident(IdentSymbol),
    Cons(ConsSymbol),
}

impl Symbol {
    pub fn is_constructor(&self) -> bool {
        match self {
            Symbol::Ident(_) => false,
            Symbol::Cons(_) => true,
        }
    }

    pub fn is_identifier(&self) -> bool {
        match self {
            Symbol::Ident(_) => true,
            Symbol::Cons(_) => false,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct StrLit(SymbToken);

trait ExtractSymbToken {
    fn extract(&self) -> SymbToken;
    fn new(id: u32, src: &str) -> Self;
}

impl ExtractSymbToken for StrLit {
    fn extract(&self) -> SymbToken {
        self.0
    }

    fn new(id: u32, _: &str) -> Self {
        Self(SymbToken(id))
    }
}

impl ExtractSymbToken for ConsSymbol {
    fn extract(&self) -> SymbToken {
        self.0
    }
    fn new(id: u32, _: &str) -> Self {
        Self(SymbToken(id))
    }
}

impl ExtractSymbToken for IdentSymbol {
    fn extract(&self) -> SymbToken {
        self.0
    }
    fn new(id: u32, _: &str) -> Self {
        Self(SymbToken(id))
    }
}

impl ExtractSymbToken for Symbol {
    fn extract(&self) -> SymbToken {
        match self {
            Symbol::Ident(ident) => ident.extract(),
            Symbol::Cons(cons) => cons.extract(),
        }
    }

    fn new(id: u32, src: &str) -> Self {
        assert!(!src.is_empty());
        let c = src.chars().next().unwrap();
        if c.is_ascii_alphabetic() && c.is_ascii_uppercase() {
            Self::Cons(ConsSymbol::new(id, src))
        } else {
            Self::Ident(IdentSymbol::new(id, src))
        }
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

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", SESSION.lock().unwrap().resolve_symbol(*self))
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", SESSION.lock().unwrap().resolve_symbol(*self))
    }
}

impl fmt::Debug for StrLit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\"{}\"",
            SESSION.lock().unwrap().resolve_strlit(*self).escape_debug()
        )
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
            let symbol = S::new(idx as u32, s);
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

    pub fn fresh(&mut self) -> S {
        let generated_name = format!("<@fresh_{}>", self.strings.len());
        self.intern(&generated_name)
    }
}

impl<S> Default for Interner<S>
where
    S: Copy + ExtractSymbToken,
{
    fn default() -> Self {
        Self::new()
    }
}

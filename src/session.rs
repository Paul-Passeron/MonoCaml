use crate::{
    lexer::interner::{Interner, StrLit, Symbol},
    source_manager::SourceManager,
};

/// Session informations, shared between all passes
pub struct Session {
    pub source_manager: SourceManager,
    pub symbol_interner: Interner<Symbol>,
    pub strlit_interner: Interner<StrLit>,
}

impl Session {
    pub fn new(source_manager: SourceManager) -> Self {
        Self {
            source_manager,
            symbol_interner: Interner::new(),
            strlit_interner: Interner::new(),
        }
    }

    pub fn intern_symbol(&mut self, symbol: &str) -> Symbol {
        self.symbol_interner.intern(symbol)
    }

    pub fn resolve_symbol(&self, symbol: Symbol) -> &str {
        self.symbol_interner.resolve(symbol)
    }

    pub fn fresh_symbol(&mut self) -> Symbol {
        self.symbol_interner.fresh()
    }

    pub fn lookup_symbol(&self, symbol: &str) -> Option<Symbol> {
        self.symbol_interner.lookup(symbol)
    }

    pub fn intern_strlit(&mut self, strlit: &str) -> StrLit {
        self.strlit_interner.intern(strlit)
    }

    pub fn resolve_strlit(&self, strlit: StrLit) -> &str {
        self.strlit_interner.resolve(strlit)
    }

    pub fn lookup_strlit(&self, strlit: &str) -> Option<StrLit> {
        self.strlit_interner.lookup(strlit)
    }
}

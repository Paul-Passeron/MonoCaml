use crate::{
    lexer::interner::{Interner, StrLit, Symbol},
    source_manager::SourceManager,
};

/// Session informations, shared between all passes
pub struct Session<'src> {
    pub source_manager: &'src mut SourceManager,
    pub symbol_interner: Interner<'src, Symbol>,
    pub strlit_interner: Interner<'src, StrLit>,
}

impl<'src> Session<'src> {
    pub fn new(source_manager: &'src mut SourceManager) -> Self {
        Self {
            symbol_interner: Interner::new(),
            strlit_interner: Interner::new(),
            source_manager,
        }
    }

    pub fn intern_symbol(&mut self, symbol: &'src str) -> Symbol {
        self.symbol_interner.intern(symbol)
    }

    pub fn resolve_symbol(&self, symbol: Symbol) -> &'src str {
        self.symbol_interner.resolve(symbol)
    }

    pub fn lookup_symbol(&mut self, symbol: &'src str) -> Option<Symbol> {
        self.symbol_interner.lookup(symbol)
    }

    pub fn intern_strlit(&mut self, strlit: &'src str) -> StrLit {
        self.strlit_interner.intern(strlit)
    }

    pub fn resolve_strlit(&self, strlit: StrLit) -> &'src str {
        self.strlit_interner.resolve(strlit)
    }

    pub fn lookup_strlit(&mut self, strlit: &'src str) -> Option<StrLit> {
        self.strlit_interner.lookup(strlit)
    }
}

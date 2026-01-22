use std::cell::RefCell;

use crate::lexer::interner::{Interner, StrLit, Symbol};

/// Session informations, shared between all passes
pub struct Session<'src> {
    pub symbol_interner: RefCell<Interner<'src, Symbol>>,
    pub strlit_interner: RefCell<Interner<'src, StrLit>>,
}

impl<'src> Session<'src> {
    pub fn new() -> Self {
        Self {
            symbol_interner: RefCell::new(Interner::new()),
            strlit_interner: RefCell::new(Interner::new()),
        }
    }

    pub fn intern_symbol(&self, symbol: &'src str) -> Symbol {
        self.symbol_interner.borrow_mut().intern(symbol)
    }

    pub fn resolve_symbol(&self, symbol: Symbol) -> &'src str {
        self.symbol_interner.borrow().resolve(symbol)
    }

    pub fn intern_strlit(&self, strlit: &'src str) -> StrLit {
        self.strlit_interner.borrow_mut().intern(strlit)
    }

    pub fn resolve_strlit(&self, strlit: StrLit) -> &'src str {
        self.strlit_interner.borrow().resolve(strlit)
    }
}

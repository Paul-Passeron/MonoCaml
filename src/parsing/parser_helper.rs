use std::marker::PhantomData;

use crate::{
    lexing::Position,
    parsing::{
        asttypes::{ArgLabel, Loc},
        longident::LongIdent,
        parsetree::Expression,
    },
};

pub enum ParenKind {
    Paren,
    Brace,
    Bracket,
}

pub enum IndexDim {
    One,
    Two,
    Three,
    Many,
}

pub struct ArrayFamily<D, I> {
    _phantom: PhantomData<(D, I)>,
}
impl<D, I> ArrayFamily<D, I> {
    pub fn name(
        _start: Position,
        _end: Position,
        _dot: D,
        _assign: bool,
        _paren_kind: ParenKind,
        _dim: IndexDim,
    ) -> Loc<LongIdent> {
        todo!()
    }

    pub fn index(
        _start: Position,
        _end: Position,
        _paren_kind: ParenKind,
    ) -> (I, Vec<(ArgLabel, Expression)>) {
        todo!()
    }
}

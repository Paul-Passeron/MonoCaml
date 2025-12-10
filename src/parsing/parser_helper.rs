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
        start: Position,
        end: Position,
        dot: D,
        assign: bool,
        paren_kind: ParenKind,
        dim: IndexDim,
    ) -> Loc<LongIdent> {
        todo!()
    }

    pub fn index(
        start: Position,
        end: Position,
        paren_kind: ParenKind,
    ) -> (I, Vec<(ArgLabel, Expression)>) {
        todo!()
    }
}

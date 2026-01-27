use crate::{
    lexer::interner::{StrLit, Symbol},
    source_manager::loc::Span,
};

pub enum TokenKind {
    Ident(Symbol),
    Intlit(u64),
    Strlit(StrLit),

    True,
    False,

    If,
    Then,
    Else,
    Match,
    With,
    Begin,
    End,
    Let,
    LetOp(Symbol),
    Rec,
    Fun,

    Plus,
    Minus,
    Star,
    Div,
    Dif,
    Eq,

    Op(Symbol),

    Bar,

    Semi,
    Comma,
    Dot,

    LPar,
    RPar,
    LBra,
    RBra,
    LSQr,
    RSQr,
}

pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

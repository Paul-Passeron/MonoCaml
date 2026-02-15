use std::fmt;

use crate::{
    lexer::interner::{StrLit, Symbol},
    resolve_strlit, resolve_symbol,
    source_manager::loc::Span,
};

#[derive(Clone, PartialEq, Eq)]
pub enum TokenKind {
    Ident(Symbol),
    Intlit(i64),
    Strlit(StrLit),
    Charlit(char),

    Wildcard,

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
    And,
    In,
    LetOp(Symbol),
    Rec,
    Fun,
    Function,
    For,
    While,
    Open,
    Do,
    Done,
    To,
    Of,
    Type,
    When,
    Mutable,

    Plus,
    Minus,
    Star,
    Div,
    Eq,
    Exclam,
    NEq,
    LT,
    GT,
    GEq,
    LEq,
    LOr,
    LAnd,
    Cons,
    Pipe,
    Arrow,
    Pound,
    Op(Symbol),

    Semi,
    Comma,
    Dot,
    Colon,

    LPar,
    RPar,
    LBra,
    RBra,
    LSqr,
    RSqr,

    Comment(String),
    PolyTypeName(Symbol),
}

impl TokenKind {
    pub fn is_comment(&self) -> bool {
        matches!(self, Self::Comment(_))
    }

    pub fn is_skip(&self) -> bool {
        self.is_comment()
    }
}

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.span, self.kind,)
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {:?}", self.span, self.kind)
    }
}

impl fmt::Debug for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Ident(_) => write!(f, "Ident: ")?,
            TokenKind::Intlit(_) => write!(f, "Intlit: ")?,
            TokenKind::Strlit(_) => write!(f, "Strlit: ")?,
            TokenKind::Charlit(_) => write!(f, "Charlit: ")?,
            TokenKind::Wildcard => write!(f, "Wildcard: ")?,
            TokenKind::LetOp(_) => write!(f, "LetOp: ")?,
            TokenKind::Op(_) => write!(f, "Op: ")?,
            TokenKind::Comment(_) => write!(f, "Comment: ")?,
            TokenKind::PolyTypeName(_) => write!(f, "PolyTypeName: ")?,
            TokenKind::True => write!(f, "True: ")?,
            TokenKind::False => write!(f, "False: ")?,
            TokenKind::If => write!(f, "If: ")?,
            TokenKind::When => write!(f, "When: ")?,
            TokenKind::Then => write!(f, "Then: ")?,
            TokenKind::Else => write!(f, "Else: ")?,
            TokenKind::Match => write!(f, "Match: ")?,
            TokenKind::With => write!(f, "With: ")?,
            TokenKind::Begin => write!(f, "Begin: ")?,
            TokenKind::End => write!(f, "End: ")?,
            TokenKind::Let => write!(f, "Let: ")?,
            TokenKind::And => write!(f, "And: ")?,
            TokenKind::In => write!(f, "In: ")?,
            TokenKind::Rec => write!(f, "Rec: ")?,
            TokenKind::Fun => write!(f, "Fun: ")?,
            TokenKind::Function => write!(f, "Function: ")?,
            TokenKind::For => write!(f, "For: ")?,
            TokenKind::While => write!(f, "While: ")?,
            TokenKind::Open => write!(f, "Open: ")?,
            TokenKind::Do => write!(f, "Do: ")?,
            TokenKind::Done => write!(f, "Done: ")?,
            TokenKind::To => write!(f, "To: ")?,
            TokenKind::Of => write!(f, "Of: ")?,
            TokenKind::Type => write!(f, "Type: ")?,
            TokenKind::Plus => write!(f, "Plus: ")?,
            TokenKind::Minus => write!(f, "Minus: ")?,
            TokenKind::Star => write!(f, "Star: ")?,
            TokenKind::Div => write!(f, "Div: ")?,
            TokenKind::Eq => write!(f, "Eq: ")?,
            TokenKind::Exclam => write!(f, "Exclam: ")?,
            TokenKind::NEq => write!(f, "NEq: ")?,
            TokenKind::LT => write!(f, "LT: ")?,
            TokenKind::GT => write!(f, "GT: ")?,
            TokenKind::GEq => write!(f, "GEq: ")?,
            TokenKind::LEq => write!(f, "LEq: ")?,
            TokenKind::LOr => write!(f, "LOr: ")?,
            TokenKind::LAnd => write!(f, "LAnd: ")?,
            TokenKind::Cons => write!(f, "Cons: ")?,
            TokenKind::Pipe => write!(f, "Pipe: ")?,
            TokenKind::Arrow => write!(f, "Arrow: ")?,
            TokenKind::Pound => write!(f, "Pound: ")?,
            TokenKind::Semi => write!(f, "Semi: ")?,
            TokenKind::Comma => write!(f, "Comma: ")?,
            TokenKind::Dot => write!(f, "Dot: ")?,
            TokenKind::Colon => write!(f, "Colon: ")?,
            TokenKind::LPar => write!(f, "LPar: ")?,
            TokenKind::RPar => write!(f, "RPar: ")?,
            TokenKind::LBra => write!(f, "LBra: ")?,
            TokenKind::RBra => write!(f, "RBra: ")?,
            TokenKind::LSqr => write!(f, "LSQr: ")?,
            TokenKind::RSqr => write!(f, "RSQr: ")?,
            TokenKind::Mutable => write!(f, "Mut: ")?,
        };

        write!(f, "{}", self)
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Op(symbol) | TokenKind::Ident(symbol) => {
                write!(f, "{}", resolve_symbol(*symbol))
            }
            TokenKind::Intlit(x) => write!(f, "{x}"),
            TokenKind::Strlit(str_lit) => {
                let lit = resolve_strlit(*str_lit);
                write!(f, "\"{}\"", lit.escape_debug())
            }
            TokenKind::PolyTypeName(symbol) => write!(f, "'{}", resolve_symbol(*symbol)),
            TokenKind::Charlit(c) => write!(f, "'{}'", c.escape_debug()),
            TokenKind::Wildcard => write!(f, "_"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::If => write!(f, "if"),
            TokenKind::When => write!(f, "when"),
            TokenKind::Then => write!(f, "then"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Match => write!(f, "match"),
            TokenKind::With => write!(f, "with"),
            TokenKind::Begin => write!(f, "begin"),
            TokenKind::End => write!(f, "end"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::And => write!(f, "and"),
            TokenKind::In => write!(f, "in"),
            TokenKind::LetOp(symbol) => write!(f, "let{}", resolve_symbol(*symbol)),
            TokenKind::Rec => write!(f, "rec"),
            TokenKind::Fun => write!(f, "fun"),
            TokenKind::Function => write!(f, "function"),
            TokenKind::For => write!(f, "for"),
            TokenKind::While => write!(f, "while"),
            TokenKind::Open => write!(f, "open"),
            TokenKind::Do => write!(f, "do"),
            TokenKind::Done => write!(f, "done"),
            TokenKind::To => write!(f, "to"),
            TokenKind::Of => write!(f, "of"),
            TokenKind::Type => write!(f, "type"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Div => write!(f, "/"),
            TokenKind::Eq => write!(f, "="),
            TokenKind::Exclam => write!(f, "!"),
            TokenKind::NEq => write!(f, "<>"),
            TokenKind::LT => write!(f, "<"),
            TokenKind::GT => write!(f, ">"),
            TokenKind::GEq => write!(f, ">="),
            TokenKind::LEq => write!(f, "<="),
            TokenKind::LOr => write!(f, "||"),
            TokenKind::LAnd => write!(f, "&&"),
            TokenKind::Cons => write!(f, "::"),
            TokenKind::Pipe => write!(f, "|"),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::Pound => write!(f, "&"),
            TokenKind::Semi => write!(f, ";"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::LPar => write!(f, "("),
            TokenKind::RPar => write!(f, ")"),
            TokenKind::LBra => write!(f, "{{"),
            TokenKind::RBra => write!(f, "}}"),
            TokenKind::LSqr => write!(f, "["),
            TokenKind::RSqr => write!(f, "]"),
            TokenKind::Mutable => write!(f, "mutable"),
            TokenKind::Comment(s) => write!(f, "{}", s.escape_debug()),
        }
    }
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn is_comment(&self) -> bool {
        self.kind.is_comment()
    }

    pub fn is_skip(&self) -> bool {
        self.kind.is_skip()
    }
}

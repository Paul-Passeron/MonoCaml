pub mod interner;
pub mod token;

pub struct Lexer<'src> {
    pub contents: &'src str,
}

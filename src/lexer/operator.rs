use crate::{
    lexer::{
        LexRes, Lexer, LexingError,
        token::{Token, TokenKind},
    },
    session::Session,
};

impl Lexer {
    fn is_operator_char(c: char) -> bool {
        matches!(
            c,
            '!' | '$'
                | '%'
                | '&'
                | '*'
                | '+'
                | '-'
                | '.'
                | '/'
                | ':'
                | '<'
                | '='
                | '>'
                | '?'
                | '@'
                | '^'
                | '|'
                | '~'
        )
    }

    pub(super) fn is_operator_start(&self, is_first: bool) -> bool {
        if let Some(c) = self.peek() {
            match c {
                '?' | '~' if is_first => self.peek_n(1).map_or(false, Self::is_operator_char),
                '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '=' | '>'
                | '@' | '^' | '|' => true,
                '?' | '~' => true, // valid as continuation
                _ => false,
            }
        } else {
            false
        }
    }

    pub(super) fn lex_operator(&mut self, session: &mut Session) -> LexRes {
        if !self.is_operator_start(true) {
            return Err(LexingError::UnexpectedChar(self.loc()));
        }
        let l = self.loc();

        while self.is_operator_start(false) {
            self.advance();
        }

        let s = &self.contents[l.offset..self.loc().offset];

        match s {
            "?" | "~" => {
                return Err(LexingError::NotAnOperator(l));
            }
            // Reserved tokens
            "+" => Ok(Token::new(TokenKind::Plus, l.span(&self.loc()))),
            "-" => Ok(Token::new(TokenKind::Minus, l.span(&self.loc()))),
            "*" => Ok(Token::new(TokenKind::Star, l.span(&self.loc()))),
            "/" => Ok(Token::new(TokenKind::Div, l.span(&self.loc()))),
            "!" => Ok(Token::new(TokenKind::Exclam, l.span(&self.loc()))),
            "=" => Ok(Token::new(TokenKind::Eq, l.span(&self.loc()))),
            "<>" => Ok(Token::new(TokenKind::NEq, l.span(&self.loc()))),
            ">" => Ok(Token::new(TokenKind::GT, l.span(&self.loc()))),
            "<" => Ok(Token::new(TokenKind::LT, l.span(&self.loc()))),
            ">=" => Ok(Token::new(TokenKind::GEq, l.span(&self.loc()))),
            "<=" => Ok(Token::new(TokenKind::LEq, l.span(&self.loc()))),
            "||" => Ok(Token::new(TokenKind::LOr, l.span(&self.loc()))),
            ":" => Ok(Token::new(TokenKind::Colon, l.span(&self.loc()))),
            "::" => Ok(Token::new(TokenKind::Cons, l.span(&self.loc()))),
            "|" => Ok(Token::new(TokenKind::Pipe, l.span(&self.loc()))),
            "->" => Ok(Token::new(TokenKind::Arrow, l.span(&self.loc()))),
            "$" => Ok(Token::new(TokenKind::Pound, l.span(&self.loc()))),

            _ => {
                let op_symbol = session.intern_symbol(s);
                Ok(Token::new(TokenKind::Op(op_symbol), l.span(&self.loc())))
            }
        }
    }
}

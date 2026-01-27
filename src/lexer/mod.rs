use std::marker::PhantomData;

use crate::{
    lexer::token::{Token, TokenKind},
    session::Session,
    source_manager::{FileId, SourceManager, loc::Loc},
};

pub mod interner;
pub mod token;

pub struct Lexer<'session> {
    pub contents: String,
    pub file: FileId,
    pub pos: usize,
    _session: PhantomData<&'session ()>,
}

pub enum LexingError {
    EOF,
    UnexpectedChar(Loc),
    UnmatchedCommentStart(Loc),
    NotAnOperator(Loc),
}

pub type LexRes = Result<Token, LexingError>;

fn is_identifier_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_identifier_continue(c: char) -> bool {
    is_identifier_start(c) || c.is_numeric()
}

impl<'session> Lexer<'session> {
    pub fn new(sm: &SourceManager, file: FileId) -> Self {
        Self {
            contents: sm.get_file(file).contents.clone(),
            file,
            pos: 0,
            _session: PhantomData,
        }
    }

    fn loc(&self) -> Loc {
        Loc::new(self.file, self.pos)
    }

    fn peek_n(&self, n: usize) -> Option<char> {
        if self.pos + n >= self.contents.len() {
            return None;
        }
        let c = self.contents[self.pos + n..]
            .chars()
            .next()
            .unwrap_or_default();
        Some(c)
    }

    fn peek(&self) -> Option<char> {
        self.peek_n(0)
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += 1;
        Some(c)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek()
            && c.is_whitespace()
        {
            let _ = self.advance();
        }
    }

    fn is_comment_start(&self) -> bool {
        self.peek_n(0) == Some('(') && self.peek_n(1) == Some('*')
    }

    fn is_comment_end(&self) -> bool {
        self.peek_n(0) == Some('*') && self.peek_n(1) == Some(')')
    }

    fn peek_or_eof(&self) -> Result<char, LexingError> {
        self.peek().ok_or_else(|| LexingError::EOF)
    }

    fn lex_comment(&mut self) -> LexRes {
        let mut s = String::new();
        let l = self.loc();
        let mut opens = vec![l];

        if !self.is_comment_start() {
            return Err(LexingError::UnexpectedChar(self.loc()));
        }
        let mut depth = 1;
        s.push(self.advance().unwrap_or_default());
        s.push(self.advance().unwrap_or_default());
        while depth > 0 && self.peek().is_some() {
            if self.is_comment_start() {
                opens.push(self.loc());
                depth += 1;
                s.push(self.advance().unwrap_or_default());
                s.push(self.advance().unwrap_or_default());
            } else if self.is_comment_end() {
                depth -= 1;
                opens.pop();
                s.push(self.advance().unwrap_or_default());
                s.push(self.advance().unwrap_or_default());
            } else {
                s.push(self.advance().unwrap_or_default());
            }
        }

        if depth != 0 {
            return Err(LexingError::UnmatchedCommentStart(opens.pop().unwrap_or(l)));
        } else {
        }

        Ok(Token::new(TokenKind::Comment(s), l.span(&self.loc())))
    }

    fn create_single_char_token(&mut self, kind: TokenKind) -> Token {
        let l = self.loc();
        self.advance();
        let span = l.span(&self.loc());
        Token::new(kind, span)
    }

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

    fn is_operator_start(&self, is_first: bool) -> bool {
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

    fn lex_operator(&mut self, session: &mut Session) -> LexRes {
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

    fn lex_identifier(&mut self, session: &mut Session) -> LexRes {
        if !is_identifier_start(self.peek_or_eof()?) {
            return Err(LexingError::UnexpectedChar(self.loc()));
        }
        let l = self.loc();
        while let Some(c) = self.peek()
            && is_identifier_continue(c)
        {
            self.advance();
        }

        let s = &self.contents[l.offset..self.loc().offset];
        match s {
            // Keywords
            "if" => Ok(Token::new(TokenKind::If, l.span(&self.loc()))),
            "then" => Ok(Token::new(TokenKind::Then, l.span(&self.loc()))),
            "else" => Ok(Token::new(TokenKind::Else, l.span(&self.loc()))),
            "for" => Ok(Token::new(TokenKind::For, l.span(&self.loc()))),
            "while" => Ok(Token::new(TokenKind::While, l.span(&self.loc()))),
            "begin" => Ok(Token::new(TokenKind::Begin, l.span(&self.loc()))),
            "end" => Ok(Token::new(TokenKind::End, l.span(&self.loc()))),
            "match" => Ok(Token::new(TokenKind::Match, l.span(&self.loc()))),
            "with" => Ok(Token::new(TokenKind::With, l.span(&self.loc()))),
            "open" => Ok(Token::new(TokenKind::Open, l.span(&self.loc()))),
            "do" => Ok(Token::new(TokenKind::Do, l.span(&self.loc()))),
            "done" => Ok(Token::new(TokenKind::Done, l.span(&self.loc()))),
            "to" => Ok(Token::new(TokenKind::To, l.span(&self.loc()))),
            "let" => {
                if self.is_operator_start(true) {
                    let _ = self.lex_operator(session)?;
                    let loc = self.loc();
                    let s = &self.contents[l.offset..loc.offset];
                    let symbol = session.intern_symbol(s);
                    Ok(Token::new(TokenKind::LetOp(symbol), l.span(&loc)))
                } else {
                    Ok(Token::new(TokenKind::Let, l.span(&self.loc())))
                }
            }
            _ => {
                let symbol = session.intern_symbol(s);
                Ok(Token::new(TokenKind::Ident(symbol), l.span(&self.loc())))
            }
        }
    }

    fn lex_string(&mut self, session: &mut Session) -> LexRes {
        let l = self.loc();
        if self.peek_or_eof()? != '"' {
            Err(LexingError::UnexpectedChar(l))
        } else {
            self.advance();
            while let Some(c) = self.peek() {
                if c == '"' {
                    self.advance();
                    break;
                }
                self.advance();
            }
            let s = &self.contents[l.offset + 1..self.loc().offset - 1];
            let strlit = session.intern_strlit(s);
            Ok(Token::new(TokenKind::Strlit(strlit), l.span(&self.loc())))
        }
    }

    fn next_token(&mut self, session: &mut Session) -> LexRes {
        self.skip_whitespace();
        let l = self.loc();
        if let Some(c) = self.peek() {
            match c {
                '(' => {
                    if let Some('*') = self.peek_n(1) {
                        self.lex_comment()
                    } else {
                        Ok(self.create_single_char_token(TokenKind::LPar))
                    }
                }
                ')' => Ok(self.create_single_char_token(TokenKind::RPar)),
                _ if self.is_operator_start(true) => {
                    // This is an operator
                    self.lex_operator(session)
                }
                '"' => self.lex_string(session),
                '?' | '~' => todo!(),
                _ if is_identifier_start(c) => self.lex_identifier(session),
                _ => Err(LexingError::UnexpectedChar(l)),
            }
        } else {
            Err(LexingError::EOF)
        }
    }
}
// ======================
/// Iterator utilities
// ======================

pub struct SkipLexer<'session> {
    session: &'session mut Session,
    inner: Lexer<'session>,
}

pub struct LexerIter<'session> {
    session: &'session mut Session,
    inner: Lexer<'session>,
}

impl<'session> Iterator for LexerIter<'session> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next_token(self.session).ok()
    }
}

impl<'session> Iterator for SkipLexer<'session> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Ok(tok) = self.inner.next_token(self.session) {
            if !tok.is_skip() {
                return Some(tok);
            }
        }

        None
    }
}

impl<'session> Lexer<'session> {
    pub fn no_skip(self, session: &'session mut Session) -> LexerIter<'session> {
        LexerIter {
            inner: self,
            session,
        }
    }
    pub fn skip(self, session: &'session mut Session) -> SkipLexer<'session> {
        SkipLexer {
            inner: self,
            session,
        }
    }
}

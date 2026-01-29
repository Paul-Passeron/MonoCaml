use crate::{
    lexer::{
        error::{LexRes, LexingError},
        token::{Token, TokenKind},
    },
    session::Session,
    source_manager::{FileId, SourceManager, loc::Loc},
};

pub mod char_lit;
pub mod error;
pub mod interner;
pub mod iter;
pub mod operator;
pub mod token;

pub struct Lexer {
    pub contents: String,
    pub file: FileId,
    pub pos: usize,
}

fn is_identifier_start(c: char) -> bool {
    is_identifier(c, true)
}

fn is_identifier_continue(c: char) -> bool {
    is_identifier(c, false)
}

fn is_identifier(c: char, is_first: bool) -> bool {
    c.is_alphabetic() || c == '_' || (!is_first && c.is_numeric())
}

impl Lexer {
    pub fn new(sm: &SourceManager, file: FileId) -> Self {
        Self {
            contents: sm.get_file(file).contents.clone(),
            file,
            pos: 0,
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
            Err(LexingError::UnmatchedCommentStart(opens.pop().unwrap_or(l)))
        } else {
            Ok(Token::new(TokenKind::Comment(s), l.span(&self.loc())))
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
            "rec" => Ok(Token::new(TokenKind::Rec, l.span(&self.loc()))),
            "fun" => Ok(Token::new(TokenKind::Fun, l.span(&self.loc()))),
            "function" => Ok(Token::new(TokenKind::Function, l.span(&self.loc()))),
            "and" => Ok(Token::new(TokenKind::And, l.span(&self.loc()))),
            "in" => Ok(Token::new(TokenKind::In, l.span(&self.loc()))),
            "let" => {
                if self.is_operator_start(true) {
                    let start = self.loc().offset;
                    let _ = self.lex_operator(session)?;
                    let loc = self.loc();
                    let s = &self.contents[start..loc.offset];
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
            let mut contents = String::new();
            while let Some(c) = self.peek() {
                if c == '\n' {
                    return Err(LexingError::UnterminatedString(l));
                }
                if c == '"' {
                    self.advance();
                    break;
                }
                contents.push(self.parse_char_content()?);
                self.peek_or_eof()?;
            }
            let strlit = session.intern_strlit(&contents);
            Ok(Token::new(TokenKind::Strlit(strlit), l.span(&self.loc())))
        }
    }

    pub fn next_token(&mut self, session: &mut Session) -> LexRes {
        self.skip_whitespace();
        let l = self.loc();
        let create_single_char_token = |this: &mut Lexer, kind| {
            let l = this.loc();
            this.advance();
            let span = l.span(&this.loc());
            Token::new(kind, span)
        };
        if let Some(c) = self.peek() {
            match c {
                '(' => {
                    if let Some('*') = self.peek_n(1) {
                        self.lex_comment()
                    } else {
                        Ok(create_single_char_token(self, TokenKind::LPar))
                    }
                }
                ')' => Ok(create_single_char_token(self, TokenKind::RPar)),
                '[' => Ok(create_single_char_token(self, TokenKind::LSqr)),
                ']' => Ok(create_single_char_token(self, TokenKind::RSqr)),
                _ if self.is_operator_start(true) => {
                    // This is an operator
                    self.lex_operator(session)
                }
                '"' => self.lex_string(session),
                '\'' => {
                    // We need to handle char lit and polymorphic type name
                    let mut length = 0;
                    while let Some(x) = self.peek_n(length + 1)
                        && x != '\''
                        && !x.is_whitespace()
                    {
                        length += 1;
                    }
                    if length == 0 {
                        return Err(LexingError::UnexpectedChar(l));
                    }

                    if self.peek_n(length + 1) == Some('\'') {
                        // We have a char literal
                        return self.lex_char_literal();
                    }

                    let slice = &self.contents[self.pos + 1..self.pos + length + 1];
                    if slice
                        .chars()
                        .enumerate()
                        .all(|(i, x)| is_identifier(x, i == 0))
                    {
                        self.advance();
                        let iden = self.lex_identifier(session)?;
                        let symb = match &iden.kind {
                            TokenKind::Ident(x) => *x,
                            _ => return Err(LexingError::KeywordInPolyTypeName(iden)),
                        };
                        Ok(Token::new(
                            TokenKind::PolyTypeName(symb),
                            l.span(&self.loc()),
                        ))
                    } else {
                        Err(LexingError::UnexpectedChar(l))
                    }
                }
                '?' | '~' => todo!(),
                _ if is_identifier_start(c) => self.lex_identifier(session),
                _ => Err(LexingError::UnexpectedChar(l)),
            }
        } else {
            Err(LexingError::EOF)
        }
    }
}

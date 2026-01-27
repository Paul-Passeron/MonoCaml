use std::{fmt, marker::PhantomData};

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
    UnterminatedString(Loc),
    KeywordInPolyTypeName(Token),
    InvalidCharLiteral(Loc),
    EmptyCharLiteral(Loc),
    InvalidEscape(Loc, char),
    InvalidUnicodeEscape(Loc, String),
    InvalidAsciiEscape(Loc, String),
    InvalidCodePoint(Loc, u32),
    UnterminatedChar(Loc),
}

pub struct LexingErrorDisplay<'a, 'b>(&'a LexingError, &'b Session);

impl<'a, 'b> fmt::Display for LexingErrorDisplay<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            LexingError::EOF => write!(f, "EOF"),
            LexingError::UnexpectedChar(loc) => {
                write!(
                    f,
                    "{}: Unexpected characted",
                    loc.display(&self.1.source_manager)
                )
            }
            LexingError::UnmatchedCommentStart(loc) => write!(
                f,
                "{}: Unmatched comment start",
                loc.display(&self.1.source_manager)
            ),
            LexingError::NotAnOperator(loc) => write!(
                f,
                "{}: Expected an operator",
                loc.display(&self.1.source_manager)
            ),
            LexingError::UnterminatedString(loc) => write!(
                f,
                "{}: String literal is not terminated",
                loc.display(&self.1.source_manager)
            ),
            LexingError::KeywordInPolyTypeName(token) => write!(
                f,
                "{}: Reserved keyword used in polymorphic type name ({})",
                token.span.split().0.display(&self.1.source_manager),
                token.kind.display(&self.1)
            ),
            LexingError::InvalidCharLiteral(loc) => write!(
                f,
                "{}: Invalid character literal",
                loc.display(&self.1.source_manager)
            ),
            LexingError::EmptyCharLiteral(loc) => write!(
                f,
                "{}: Empty character literal",
                loc.display(&self.1.source_manager)
            ),
            LexingError::InvalidEscape(loc, c) => {
                write!(
                    f,
                    "{}: Invalid escape character '{}'",
                    loc.display(&self.1.source_manager),
                    c.escape_debug()
                )
            }
            LexingError::InvalidUnicodeEscape(loc, s) => write!(
                f,
                "{}: Invalid unicode escape {}",
                loc.display(&self.1.source_manager),
                s
            ),
            LexingError::InvalidAsciiEscape(loc, s) => write!(
                f,
                "{}: Invalid ascii escape {}",
                loc.display(&self.1.source_manager),
                s
            ),
            LexingError::InvalidCodePoint(loc, cp) => write!(
                f,
                "{}: Invalid code point {}",
                loc.display(&self.1.source_manager),
                cp
            ),
            LexingError::UnterminatedChar(loc) => {
                write!(
                    f,
                    "{}: Unterminated char literal",
                    loc.display(&self.1.source_manager),
                )
            }
        }
    }
}

impl LexingError {
    pub fn display<'a, 'b>(&'a self, s: &'b Session) -> LexingErrorDisplay<'a, 'b> {
        LexingErrorDisplay(self, s)
    }
}

pub type LexRes = Result<Token, LexingError>;

fn is_identifier_start(c: char) -> bool {
    is_identifier(c, true)
}

fn is_identifier_continue(c: char) -> bool {
    is_identifier(c, false)
}

fn is_identifier(c: char, is_first: bool) -> bool {
    c.is_alphabetic() || c == '_' || (!is_first && c.is_numeric())
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
            while let Some(c) = self.peek() {
                if c == '\n' {
                    return Err(LexingError::UnterminatedString(l));
                }
                if c == '"' {
                    self.advance();
                    break;
                }
                self.advance();
                self.peek_or_eof()?;
            }
            let s = &self.contents[l.offset + 1..self.loc().offset - 1];
            let strlit = session.intern_strlit(s);
            Ok(Token::new(TokenKind::Strlit(strlit), l.span(&self.loc())))
        }
    }

    pub fn parse_char_content(&mut self) -> Result<char, LexingError> {
        match self.advance() {
            None => Err(LexingError::EmptyCharLiteral(self.loc().previous())),
            Some('\\') => self.parse_escape_sequence(),
            Some(c) => Ok(c),
        }
    }

    fn parse_escape_sequence(&mut self) -> Result<char, LexingError> {
        match self.advance() {
            None => Err(LexingError::InvalidEscape(self.loc().previous(), ' ')),
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('t') => Ok('\t'),
            Some('\\') => Ok('\\'),
            Some('0') => Ok('\0'),
            Some('\'') => Ok('\''),
            Some('"') => Ok('"'),
            Some('x') => self.parse_ascii_escape(),
            Some('u') => self.parse_unicode_escape(),
            Some(c) => Err(LexingError::InvalidEscape(self.loc(), c)),
        }
    }

    fn parse_ascii_escape(&mut self) -> Result<char, LexingError> {
        let l = self.loc();
        // Expect exactly 2 hex digits
        let mut hex = String::with_capacity(2);

        for _ in 0..2 {
            match self.advance() {
                Some(c) if c.is_ascii_hexdigit() => hex.push(c),
                Some(c) => {
                    return Err(LexingError::InvalidAsciiEscape(
                        self.loc().previous(),
                        format!("\\x{hex}{c}"),
                    ));
                }
                None => {
                    return Err(LexingError::InvalidAsciiEscape(
                        self.loc().previous(),
                        format!("\\x{hex}"),
                    ));
                }
            }
        }

        let value = u8::from_str_radix(&hex, 16)
            .map_err(|_| LexingError::InvalidAsciiEscape(l, format!("\\x{hex}")))?;

        // ASCII escape must be in range 0x00-0x7F
        if value > 0x7F {
            return Err(LexingError::InvalidAsciiEscape(
                l,
                format!("\\x{hex} (value {value:#x} exceeds 0x7F)"),
            ));
        }

        Ok(value as char)
    }

    fn parse_unicode_escape(&mut self) -> Result<char, LexingError> {
        // Expect opening brace
        match self.advance() {
            Some('{') => {}
            Some(c) => {
                return Err(LexingError::InvalidUnicodeEscape(
                    self.loc().previous(),
                    format!("\\u{c}"),
                ));
            }
            None => {
                return Err(LexingError::InvalidUnicodeEscape(
                    self.loc().previous(),
                    "\\u".to_string(),
                ));
            }
        }

        // Collect 1-6 hex digits
        let mut hex = String::with_capacity(6);

        let l = self.loc();

        loop {
            match self.peek() {
                Some('}') => {
                    self.advance();
                    break;
                }
                Some(c) if c.is_ascii_hexdigit() && hex.len() < 6 => {
                    hex.push(c);
                    self.advance();
                }
                Some(c) if c.is_ascii_hexdigit() => {
                    return Err(LexingError::InvalidUnicodeEscape(
                        self.loc().previous(),
                        format!("\\u{{{hex}... (too many digits)"),
                    ));
                }
                Some(c) => {
                    return Err(LexingError::InvalidUnicodeEscape(
                        self.loc().previous(),
                        format!("\\u{{{hex}{c}"),
                    ));
                }
                None => {
                    return Err(LexingError::InvalidUnicodeEscape(
                        self.loc().previous(),
                        format!("\\u{{{hex} (missing closing brace)"),
                    ));
                }
            }
        }

        if hex.is_empty() {
            return Err(LexingError::InvalidUnicodeEscape(
                l,
                "\\u{} (empty)".to_string(),
            ));
        }

        let code_point = u32::from_str_radix(&hex, 16)
            .map_err(|_| LexingError::InvalidUnicodeEscape(l, format!("\\u{{{hex}}}")))?;

        char::from_u32(code_point).ok_or(LexingError::InvalidCodePoint(l, code_point))
    }

    fn lex_char_literal(&mut self) -> LexRes {
        if self.peek_or_eof()? != '\'' {
            return Err(LexingError::UnexpectedChar(self.loc()));
        }

        let l = self.loc();
        self.advance();

        let c = self.parse_char_content()?;

        if self.peek_or_eof()? != '\'' {
            return Err(LexingError::UnterminatedChar(self.loc()));
        }
        self.advance();

        Ok(Token::new(TokenKind::Charlit(c), l.span(&self.loc())))
    }

    pub fn next_token(&mut self, session: &mut Session) -> LexRes {
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
                '[' => Ok(self.create_single_char_token(TokenKind::LSqr)),
                ']' => Ok(self.create_single_char_token(TokenKind::RSqr)),
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

use crate::lexer::{
    LexRes, Lexer, LexingError,
    token::{Token, TokenKind},
};

impl Lexer {
    pub(super) fn parse_char_content(&mut self) -> Result<char, LexingError> {
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

    pub(super) fn lex_char_literal(&mut self) -> LexRes {
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
}

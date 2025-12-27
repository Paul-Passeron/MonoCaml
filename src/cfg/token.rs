use crate::helpers::unique::Token;

impl From<Token<super::Label, usize>> for super::Label {
    fn from(token: Token<super::Label, usize>) -> Self {
        Self(token.inner)
    }
}

impl From<Token<super::FunName, usize>> for super::FunName {
    fn from(token: Token<super::FunName, usize>) -> Self {
        Self(token.inner)
    }
}

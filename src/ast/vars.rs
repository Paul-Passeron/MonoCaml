use crate::helpers::unique::Token;

// This allows Unique to work with Var through the generic Token
impl From<Token<super::Var, usize>> for super::Var {
    fn from(token: Token<super::Var, usize>) -> Self {
        Self(token.inner)
    }
}

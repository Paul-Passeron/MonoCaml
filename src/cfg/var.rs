use crate::helpers::unique::{Extractable, Token, Unique, Use};
#[allow(unused)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CfgVar(usize);

impl CfgVar {
    pub fn fresh() -> Self {
        Unique::<Self>::fresh()
    }

    pub fn reset() {
        Unique::<Self>::reset();
    }
}

pub type CfgVarUse = Use<CfgVar>;

impl Extractable for CfgVar {
    fn extract(&self) -> usize {
        self.0
    }
}

pub trait VarKind {}
impl VarKind for CfgVar {}
impl VarKind for CfgVarUse {}

impl From<Token<CfgVar, usize>> for CfgVar {
    fn from(token: Token<CfgVar, usize>) -> Self {
        Self(token.inner)
    }
}

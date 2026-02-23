use crate::poly_ir::{TypeId, TypeParamId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVarId(pub u32);

#[derive(Default, Debug, Clone)]
pub enum Type {
    #[default]
    Infer,
    Param(TypeParamId),
    Var(TypeVarId),
    Arrow {
        // TODO: label
        param: Box<Type>,
        result: Box<Type>,
    },
    Tuple(Vec<Type>),
    Constr {
        id: TypeId,
        args: Vec<Type>,
    },
}

impl Type {
    pub fn is_infer(&self) -> bool {
        matches!(self, Self::Infer)
    }

    pub fn is_param(&self) -> bool {
        matches!(self, Self::Param(..))
    }

    pub fn is_arrow(&self) -> bool {
        matches!(self, Self::Arrow { .. })
    }

    pub fn is_tuple(&self) -> bool {
        matches!(self, Self::Tuple(..))
    }

    pub fn is_constr(&self) -> bool {
        matches!(self, Self::Constr { .. })
    }
}

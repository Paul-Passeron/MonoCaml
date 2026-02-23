use crate::{inference::InferVarId, intern_symbol, lexer::interner::Symbol, poly_ir::id::Id};

pub type SolvedTy = MonoTy;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MonoTy {
    Var(TyVar),
    Con(TyCon),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyForall {
    pub tyvars: Vec<TyVar>,
    pub ty: Box<MonoTy>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyVar {
    pub id: InferVarId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyCon {
    pub name: Symbol,
    pub args: Vec<Id<MonoTy>>,
}

impl MonoTy {
    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var(_))
    }

    pub fn is_con(&self) -> bool {
        matches!(self, Self::Con(_))
    }

    pub fn as_var(&self) -> Option<&TyVar> {
        match self {
            MonoTy::Var(ty_var) => Some(ty_var),
            _ => None,
        }
    }

    pub fn as_con(&self) -> Option<&TyCon> {
        match self {
            MonoTy::Con(ty_con) => Some(ty_con),
            _ => None,
        }
    }

    pub fn into_var(self) -> Option<TyVar> {
        match self {
            MonoTy::Var(ty_var) => Some(ty_var),
            _ => None,
        }
    }

    pub fn into_con(self) -> Option<TyCon> {
        match self {
            MonoTy::Con(ty_con) => Some(ty_con),
            _ => None,
        }
    }

    pub fn tuple_ty(args: Vec<Id<Self>>) -> Self {
        MonoTy::Con(TyCon {
            name: intern_symbol("*"),
            args,
        })
    }

    pub fn list_ty(ty: Id<Self>) -> Self {
        MonoTy::Con(TyCon {
            name: intern_symbol("list"),
            args: vec![ty],
        })
    }

    pub fn func_ty(arg: Id<Self>, ret: Id<Self>) -> Self {
        MonoTy::Con(TyCon {
            name: intern_symbol("->"),
            args: vec![arg, ret],
        })
    }

    pub fn option_ty(ty: Id<Self>) -> Self {
        MonoTy::Con(TyCon {
            name: intern_symbol("option"),
            args: vec![ty],
        })
    }

    pub fn result_ty(ok: Id<Self>, err: Id<Self>) -> Self {
        MonoTy::Con(TyCon {
            name: intern_symbol("result"),
            args: vec![ok, err],
        })
    }

    pub fn int_ty() -> Self {
        MonoTy::Con(TyCon {
            name: intern_symbol("int"),
            args: vec![],
        })
    }

    pub fn char_ty() -> Self {
        MonoTy::Con(TyCon {
            name: intern_symbol("char"),
            args: vec![],
        })
    }

    pub fn string_ty() -> Self {
        MonoTy::Con(TyCon {
            name: intern_symbol("string"),
            args: vec![],
        })
    }

    pub fn float_ty() -> Self {
        MonoTy::Con(TyCon {
            name: intern_symbol("float"),
            args: vec![],
        })
    }

    pub fn bool_ty() -> Self {
        MonoTy::Con(TyCon {
            name: intern_symbol("bool"),
            args: vec![],
        })
    }
}

impl Into<MonoTy> for TyCon {
    fn into(self) -> MonoTy {
        MonoTy::Con(self)
    }
}

impl Into<MonoTy> for TyVar {
    fn into(self) -> MonoTy {
        MonoTy::Var(self)
    }
}

impl TyVar {
    pub fn new(id: InferVarId) -> Self {
        Self { id }
    }
}

impl TyForall {
    pub fn new(vars: Vec<impl Into<TyVar>>, ty: impl Into<MonoTy>) -> Self {
        Self {
            tyvars: vars.into_iter().map(Into::into).collect(),
            ty: Box::new(ty.into()),
        }
    }
}

impl TyCon {
    pub fn new(name: Symbol, args: Vec<Id<MonoTy>>) -> Self {
        Self {
            name,
            args: args.into_iter().map(Into::into).collect(),
        }
    }

    pub fn from_name(name: impl Into<String>, args: Vec<Id<MonoTy>>) -> Self {
        let name = name.into();
        let symb = intern_symbol(name.as_str());
        Self::new(symb, args)
    }
}

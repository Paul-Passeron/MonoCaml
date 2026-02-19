use crate::{intern_symbol, lexer::interner::Symbol, poly_ir::type_expr::TypeVarId};

pub type SolvedTy = MonoTy;

#[derive(Debug, Clone)]
pub enum MonoTy {
    Var(TyVar),
    Con(TyCon),
    Forall(TyForall),
}

#[derive(Debug, Clone)]
pub struct TyForall {
    pub tyvars: Vec<TyVar>,
    pub ty: Box<MonoTy>,
}

#[derive(Debug, Clone, Copy)]
pub struct TyVar {
    pub id: TypeVarId,
}

#[derive(Debug, Clone)]
pub struct TyCon {
    pub name: Symbol,
    pub args: Vec<MonoTy>,
}

impl MonoTy {
    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var(_))
    }

    pub fn is_con(&self) -> bool {
        matches!(self, Self::Con(_))
    }

    pub fn is_forall(&self) -> bool {
        matches!(self, Self::Forall(_))
    }

    pub fn as_var(&self) -> Option<TyVar> {
        match self {
            MonoTy::Var(ty_var) => Some(*ty_var),
            _ => None,
        }
    }

    pub fn as_con(&self) -> Option<&TyCon> {
        match self {
            MonoTy::Con(ty_con) => Some(ty_con),
            _ => None,
        }
    }

    pub fn as_forall(&self) -> Option<&TyForall> {
        match self {
            MonoTy::Forall(forall) => Some(forall),
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

    pub fn into_forall(self) -> Option<TyForall> {
        match self {
            MonoTy::Forall(forall) => Some(forall),
            _ => None,
        }
    }

    pub fn list_ty(ty: Self) -> Self {
        MonoTy::Con(TyCon {
            name: intern_symbol("list"),
            args: vec![ty],
        })
    }

    pub fn func_ty(arg: Self, ret: Self) -> Self {
        MonoTy::Con(TyCon {
            name: intern_symbol("->"),
            args: vec![arg, ret],
        })
    }

    pub fn option_ty(ty: Self) -> Self {
        MonoTy::Con(TyCon {
            name: intern_symbol("option"),
            args: vec![ty],
        })
    }

    pub fn result_ty(ok: Self, err: Self) -> Self {
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

impl Into<TyVar> for TypeVarId {
    fn into(self) -> TyVar {
        TyVar { id: self }
    }
}

impl Into<MonoTy> for TypeVarId {
    fn into(self) -> MonoTy {
        MonoTy::Var(self.into())
    }
}

impl Into<MonoTy> for TyForall {
    fn into(self) -> MonoTy {
        MonoTy::Forall(self)
    }
}

impl TyVar {
    pub fn new(id: TypeVarId) -> Self {
        id.into()
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
    pub fn new(name: Symbol, args: Vec<impl Into<MonoTy>>) -> Self {
        Self {
            name,
            args: args.into_iter().map(Into::into).collect(),
        }
    }

    pub fn from_name(name: impl Into<String>, args: Vec<impl Into<MonoTy>>) -> Self {
        let name = name.into();
        let symb = intern_symbol(name.as_str());
        Self::new(symb, args)
    }
}

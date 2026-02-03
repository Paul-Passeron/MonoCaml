use crate::{
    mono_ir::{Ast, types::AstCtx},
    resolved::poly_ir::structure::PolyStruct,
    session::Session,
};

pub(self) type Err = ();

mod implem;

pub fn poly_to_mono(_poly: &PolyStruct, _session: &mut Session) -> Result<(Ast<()>, AstCtx), Err> {
    todo!()
}

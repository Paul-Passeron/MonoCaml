use crate::{
    parse_tree::structure::Structure,
    resolved::poly_ir::{PolyIR, structure::PolyStruct},
    session::Session,
};

pub(self) type Err = ();

mod implem;

pub fn parse_to_poly(parsed: &Structure, session: &mut Session) -> Result<PolyStruct, Err> {
    PolyIR::structure_to_poly(parsed, session)
}

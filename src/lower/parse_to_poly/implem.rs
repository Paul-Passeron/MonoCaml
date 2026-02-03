use crate::{
    parse_tree::structure::Structure,
    resolved::poly_ir::{PolyIR, structure::PolyStruct},
    session::Session,
};

use super::Err;

impl PolyIR {
    pub fn structure_to_poly(parsed: &Structure, session: &mut Session) -> Result<PolyStruct, Err> {
        todo!(
            "transform to poly:\n{}",
            parsed
                .iter()
                .map(|x| x.desc.display(session, 0).to_string())
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

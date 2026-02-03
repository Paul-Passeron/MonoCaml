use crate::{
    cfg::Program,
    lower::{mono_to_cfg::MonoToCfg, parse_to_poly::parse_to_poly, poly_to_mono::poly_to_mono},
    parse_tree::structure::Structure,
    session::Session,
};

type Err = ();

pub fn parse_to_cfg(parsed: &Structure, session: &mut Session) -> Result<Program, Err> {
    let poly = match parse_to_poly(parsed, session) {
        Ok(poly) => poly,
        Result::Err(_) => todo!(),
    };
    let (ast, ctx) = match poly_to_mono(&poly, session) {
        Ok(mono) => mono,
        Result::Err(_) => todo!(),
    };

    Ok(MonoToCfg::compile(ast, ctx))
}

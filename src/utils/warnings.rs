use crate::lexing::Position;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Location {
    start: Position,
    end: Position,
    ghost: bool,
}

impl Location {
    pub fn ghost_loc_in_file<T: ToString>(name: T) -> Self {
        let dummy_pos = Position::dummy().with_file_name(name);
        Self {
            start: dummy_pos.clone(),
            end: dummy_pos,
            ghost: true,
        }
    }
}

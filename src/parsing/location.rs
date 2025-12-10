use crate::lexing::Position;

pub struct Location {
    start: Position,
    end: Position,
    ghost: bool,
}

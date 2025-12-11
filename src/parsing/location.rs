use crate::utils::warnings;

pub type Location = warnings::Location;

impl Location {
    pub fn in_file<T: ToString>(file_name: T) -> Self {
        warnings::Location::ghost_loc_in_file(file_name)
    }

    pub fn none() -> Self {
        Self::in_file("_none_")
    }

    pub fn is_none(&self) -> bool {
        self == &Self::none()
    }
}

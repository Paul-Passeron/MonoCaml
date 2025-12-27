use crate::{
    cfg::{FunName, Label},
    helpers::unique::Extractable,
};

impl Extractable for Label {
    fn extract(&self) -> usize {
        self.0
    }
}

impl Extractable for FunName {
    fn extract(&self) -> usize {
        self.0
    }
}

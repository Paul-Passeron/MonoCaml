pub trait NativeInt: ToString + Clone + Copy {}

#[derive(Clone, Copy)]
pub struct Native32(pub i32);

#[derive(Clone, Copy)]
pub struct Native64(pub i64);

impl NativeInt for Native32 {}
impl ToString for Native32 {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

impl NativeInt for Native64 {}
impl ToString for Native64 {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

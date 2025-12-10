pub trait NativeInt {}
pub struct Native32(pub i32);
pub struct Native64(pub i64);
impl NativeInt for Native32 {}
impl NativeInt for Native64 {}

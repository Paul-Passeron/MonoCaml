use super::{Token, Unique};
use std::marker::PhantomData;

pub struct MyU<T>(usize, PhantomData<T>);

impl<T> MyU<T> {
    pub fn extract(&self) -> usize {
        self.0
    }
}

impl<T: 'static> From<Token<MyU<T>, usize>> for MyU<T> {
    fn from(token: Token<MyU<T>, usize>) -> Self {
        Self(token.inner, PhantomData)
    }
}

#[test]
pub fn test_multiple_uniques() {
    pub struct MyU1Marker;
    pub struct MyU2Marker;
    pub type MyU1 = MyU<MyU1Marker>;
    pub type MyU2 = MyU<MyU2Marker>;
    let u11 = Unique::<MyU1>::fresh();
    let u12 = Unique::<MyU1>::fresh();
    let u21 = Unique::<MyU2>::fresh();
    let u22 = Unique::<MyU2>::fresh();
    let u13 = Unique::<MyU1>::fresh();
    let u23 = Unique::<MyU2>::fresh();
    assert_eq!(u11.extract(), 0);
    assert_eq!(u12.extract(), 1);
    assert_eq!(u21.extract(), 0);
    assert_eq!(u22.extract(), 1);
    assert_eq!(u13.extract(), 2);
    assert_eq!(u23.extract(), 2);
}

#[test]
pub fn test_single_unique() {
    struct MyU3Marker;
    type MyU3 = MyU<MyU3Marker>;
    for i in 0..10000 {
        assert_eq!(Unique::<MyU3>::fresh().extract(), i);
    }
}

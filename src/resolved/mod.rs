pub mod def;
pub mod poly_ir;

pub(self) mod uniqueness {
    use crate::helpers::unique::Token;
    use crate::helpers::unique::Unique;
    use std::marker::PhantomData;

    #[derive(Debug, Clone, Hash, PartialEq, Eq)]
    pub struct TypeDefMarker;

    #[derive(Debug, Clone, Hash, PartialEq, Eq)]
    pub struct TypeVarMarker;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Uniklon<Marker>(usize, PhantomData<Marker>);

    impl<T> From<Token<Uniklon<T>, usize>> for Uniklon<T> {
        fn from(value: Token<Uniklon<T>, usize>) -> Self {
            Self(value.inner, PhantomData)
        }
    }

    impl<T: 'static> Uniklon<T> {
        pub fn fresh() -> Self {
            Unique::<Self>::fresh()
        }

        pub fn extract(&self) -> usize {
            self.0
        }

        pub fn reset() {
            Unique::<Self>::reset();
        }
    }
}

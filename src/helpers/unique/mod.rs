use super::gen_int::GenInt;
use lazy_static::lazy_static;
use num_traits::Zero;
use std::{any::TypeId, collections::HashMap, marker::PhantomData, sync::Mutex};

#[cfg(test)]
mod test;

pub struct Unique<T: 'static, S = usize>
where
    GenInt: From<S>,
{
    _ghost: PhantomData<(T, S)>,
}

lazy_static! {
    static ref UNIQUE_MAP: Mutex<HashMap<TypeId, GenInt>> = Mutex::new(HashMap::new());
}

pub(crate) struct Token<T, S>
where
    GenInt: From<S>,
{
    _ghost: PhantomData<T>,
    pub(crate) inner: S,
}

impl<T, S> From<S> for Token<T, S>
where
    GenInt: From<S>,
{
    fn from(inner: S) -> Self {
        Self {
            inner,
            _ghost: PhantomData,
        }
    }
}

#[allow(private_bounds)]
impl<T: 'static, S: 'static> Unique<T, S>
where
    GenInt: From<S> + Into<S>,
    S: Zero + Clone,
    Token<T, S>: Into<T>,
{
    pub fn fresh() -> T {
        let id = TypeId::of::<(T, S)>();

        let mut m = UNIQUE_MAP.lock().unwrap();

        let value = if m.contains_key(&id) {
            let r = m.get_mut(&id).unwrap();
            r.incr();
            (*r).into()
        } else {
            let z = S::zero();
            m.insert(id, GenInt::from(z.clone()));
            z
        };

        Token::from(value).into()
    }
}

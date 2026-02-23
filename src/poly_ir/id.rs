use std::{fmt::Debug, hash::Hash, marker::PhantomData};

pub struct Id<T> {
    pub raw: u32,
    pub _marker: PhantomData<fn() -> T>,
}

impl<T> std::fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = std::any::type_name::<T>().split("::").last().unwrap();
        write!(f, "Id::{name}({})", self.raw)
    }
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Id<T> {}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<T> Eq for Id<T> {}

impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.raw.cmp(&other.raw)
    }
}

impl<T> Id<T> {
    pub fn raw(&self) -> u32 {
        self.raw
    }
}

impl<T> Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.raw.hash(state);
    }
}

#[derive(Debug)]
pub struct Arena<T> {
    entries: Vec<T>,
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn alloc(&mut self, value: T) -> Id<T> {
        let id = Id {
            raw: self.entries.len() as u32,
            _marker: PhantomData,
        };
        self.entries.push(value);
        id
    }

    pub fn get(&self, id: Id<T>) -> &T {
        &self.entries[id.raw() as usize]
    }

    pub fn get_mut(&mut self, id: Id<T>) -> &mut T {
        &mut self.entries[id.raw() as usize]
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (Id<T>, &T)> {
        self.entries.iter().enumerate().map(|(i, v)| {
            (
                Id {
                    raw: i as u32,
                    _marker: PhantomData,
                },
                v,
            )
        })
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> std::ops::Index<Id<T>> for Arena<T> {
    type Output = T;

    fn index(&self, index: Id<T>) -> &Self::Output {
        self.get(index)
    }
}

impl<T> std::ops::IndexMut<Id<T>> for Arena<T> {
    fn index_mut(&mut self, index: Id<T>) -> &mut Self::Output {
        self.get_mut(index)
    }
}

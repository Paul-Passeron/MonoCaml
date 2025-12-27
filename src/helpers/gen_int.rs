#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum GenInt {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    ISize(isize),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    USize(usize),
}

impl From<i8> for GenInt {
    fn from(value: i8) -> Self {
        Self::I8(value)
    }
}
impl From<i16> for GenInt {
    fn from(value: i16) -> Self {
        Self::I16(value)
    }
}
impl From<i32> for GenInt {
    fn from(value: i32) -> Self {
        Self::I32(value)
    }
}
impl From<i64> for GenInt {
    fn from(value: i64) -> Self {
        Self::I64(value)
    }
}
impl From<i128> for GenInt {
    fn from(value: i128) -> Self {
        Self::I128(value)
    }
}
impl From<isize> for GenInt {
    fn from(value: isize) -> Self {
        Self::ISize(value)
    }
}
impl From<u8> for GenInt {
    fn from(value: u8) -> Self {
        Self::U8(value)
    }
}
impl From<u16> for GenInt {
    fn from(value: u16) -> Self {
        Self::U16(value)
    }
}
impl From<u32> for GenInt {
    fn from(value: u32) -> Self {
        Self::U32(value)
    }
}
impl From<u64> for GenInt {
    fn from(value: u64) -> Self {
        Self::U64(value)
    }
}
impl From<u128> for GenInt {
    fn from(value: u128) -> Self {
        Self::U128(value)
    }
}
impl From<usize> for GenInt {
    fn from(value: usize) -> Self {
        Self::USize(value)
    }
}

impl Into<i8> for GenInt {
    fn into(self) -> i8 {
        match self {
            GenInt::I8(x) => x as i8,
            GenInt::I16(x) => x as i8,
            GenInt::I32(x) => x as i8,
            GenInt::I64(x) => x as i8,
            GenInt::I128(x) => x as i8,
            GenInt::ISize(x) => x as i8,
            GenInt::U8(x) => x as i8,
            GenInt::U16(x) => x as i8,
            GenInt::U32(x) => x as i8,
            GenInt::U64(x) => x as i8,
            GenInt::U128(x) => x as i8,
            GenInt::USize(x) => x as i8,
        }
    }
}
impl Into<i16> for GenInt {
    fn into(self) -> i16 {
        match self {
            GenInt::I8(x) => x as i16,
            GenInt::I16(x) => x as i16,
            GenInt::I32(x) => x as i16,
            GenInt::I64(x) => x as i16,
            GenInt::I128(x) => x as i16,
            GenInt::ISize(x) => x as i16,
            GenInt::U8(x) => x as i16,
            GenInt::U16(x) => x as i16,
            GenInt::U32(x) => x as i16,
            GenInt::U64(x) => x as i16,
            GenInt::U128(x) => x as i16,
            GenInt::USize(x) => x as i16,
        }
    }
}
impl Into<i32> for GenInt {
    fn into(self) -> i32 {
        match self {
            GenInt::I8(x) => x as i32,
            GenInt::I16(x) => x as i32,
            GenInt::I32(x) => x as i32,
            GenInt::I64(x) => x as i32,
            GenInt::I128(x) => x as i32,
            GenInt::ISize(x) => x as i32,
            GenInt::U8(x) => x as i32,
            GenInt::U16(x) => x as i32,
            GenInt::U32(x) => x as i32,
            GenInt::U64(x) => x as i32,
            GenInt::U128(x) => x as i32,
            GenInt::USize(x) => x as i32,
        }
    }
}
impl Into<i64> for GenInt {
    fn into(self) -> i64 {
        match self {
            GenInt::I8(x) => x as i64,
            GenInt::I16(x) => x as i64,
            GenInt::I32(x) => x as i64,
            GenInt::I64(x) => x as i64,
            GenInt::I128(x) => x as i64,
            GenInt::ISize(x) => x as i64,
            GenInt::U8(x) => x as i64,
            GenInt::U16(x) => x as i64,
            GenInt::U32(x) => x as i64,
            GenInt::U64(x) => x as i64,
            GenInt::U128(x) => x as i64,
            GenInt::USize(x) => x as i64,
        }
    }
}
impl Into<i128> for GenInt {
    fn into(self) -> i128 {
        match self {
            GenInt::I8(x) => x as i128,
            GenInt::I16(x) => x as i128,
            GenInt::I32(x) => x as i128,
            GenInt::I64(x) => x as i128,
            GenInt::I128(x) => x as i128,
            GenInt::ISize(x) => x as i128,
            GenInt::U8(x) => x as i128,
            GenInt::U16(x) => x as i128,
            GenInt::U32(x) => x as i128,
            GenInt::U64(x) => x as i128,
            GenInt::U128(x) => x as i128,
            GenInt::USize(x) => x as i128,
        }
    }
}
impl Into<isize> for GenInt {
    fn into(self) -> isize {
        match self {
            GenInt::I8(x) => x as isize,
            GenInt::I16(x) => x as isize,
            GenInt::I32(x) => x as isize,
            GenInt::I64(x) => x as isize,
            GenInt::I128(x) => x as isize,
            GenInt::ISize(x) => x as isize,
            GenInt::U8(x) => x as isize,
            GenInt::U16(x) => x as isize,
            GenInt::U32(x) => x as isize,
            GenInt::U64(x) => x as isize,
            GenInt::U128(x) => x as isize,
            GenInt::USize(x) => x as isize,
        }
    }
}
impl Into<u8> for GenInt {
    fn into(self) -> u8 {
        match self {
            GenInt::I8(x) => x as u8,
            GenInt::I16(x) => x as u8,
            GenInt::I32(x) => x as u8,
            GenInt::I64(x) => x as u8,
            GenInt::I128(x) => x as u8,
            GenInt::ISize(x) => x as u8,
            GenInt::U8(x) => x as u8,
            GenInt::U16(x) => x as u8,
            GenInt::U32(x) => x as u8,
            GenInt::U64(x) => x as u8,
            GenInt::U128(x) => x as u8,
            GenInt::USize(x) => x as u8,
        }
    }
}
impl Into<u16> for GenInt {
    fn into(self) -> u16 {
        match self {
            GenInt::I8(x) => x as u16,
            GenInt::I16(x) => x as u16,
            GenInt::I32(x) => x as u16,
            GenInt::I64(x) => x as u16,
            GenInt::I128(x) => x as u16,
            GenInt::ISize(x) => x as u16,
            GenInt::U8(x) => x as u16,
            GenInt::U16(x) => x as u16,
            GenInt::U32(x) => x as u16,
            GenInt::U64(x) => x as u16,
            GenInt::U128(x) => x as u16,
            GenInt::USize(x) => x as u16,
        }
    }
}
impl Into<u32> for GenInt {
    fn into(self) -> u32 {
        match self {
            GenInt::I8(x) => x as u32,
            GenInt::I16(x) => x as u32,
            GenInt::I32(x) => x as u32,
            GenInt::I64(x) => x as u32,
            GenInt::I128(x) => x as u32,
            GenInt::ISize(x) => x as u32,
            GenInt::U8(x) => x as u32,
            GenInt::U16(x) => x as u32,
            GenInt::U32(x) => x as u32,
            GenInt::U64(x) => x as u32,
            GenInt::U128(x) => x as u32,
            GenInt::USize(x) => x as u32,
        }
    }
}
impl Into<u64> for GenInt {
    fn into(self) -> u64 {
        match self {
            GenInt::I8(x) => x as u64,
            GenInt::I16(x) => x as u64,
            GenInt::I32(x) => x as u64,
            GenInt::I64(x) => x as u64,
            GenInt::I128(x) => x as u64,
            GenInt::ISize(x) => x as u64,
            GenInt::U8(x) => x as u64,
            GenInt::U16(x) => x as u64,
            GenInt::U32(x) => x as u64,
            GenInt::U64(x) => x as u64,
            GenInt::U128(x) => x as u64,
            GenInt::USize(x) => x as u64,
        }
    }
}
impl Into<u128> for GenInt {
    fn into(self) -> u128 {
        match self {
            GenInt::I8(x) => x as u128,
            GenInt::I16(x) => x as u128,
            GenInt::I32(x) => x as u128,
            GenInt::I64(x) => x as u128,
            GenInt::I128(x) => x as u128,
            GenInt::ISize(x) => x as u128,
            GenInt::U8(x) => x as u128,
            GenInt::U16(x) => x as u128,
            GenInt::U32(x) => x as u128,
            GenInt::U64(x) => x as u128,
            GenInt::U128(x) => x as u128,
            GenInt::USize(x) => x as u128,
        }
    }
}
impl Into<usize> for GenInt {
    fn into(self) -> usize {
        match self {
            GenInt::I8(x) => x as usize,
            GenInt::I16(x) => x as usize,
            GenInt::I32(x) => x as usize,
            GenInt::I64(x) => x as usize,
            GenInt::I128(x) => x as usize,
            GenInt::ISize(x) => x as usize,
            GenInt::U8(x) => x as usize,
            GenInt::U16(x) => x as usize,
            GenInt::U32(x) => x as usize,
            GenInt::U64(x) => x as usize,
            GenInt::U128(x) => x as usize,
            GenInt::USize(x) => x as usize,
        }
    }
}

impl GenInt {
    pub fn incr(&mut self) {
        match self {
            GenInt::I8(x) => *x += 1,
            GenInt::I16(x) => *x += 1,
            GenInt::I32(x) => *x += 1,
            GenInt::I64(x) => *x += 1,
            GenInt::I128(x) => *x += 1,
            GenInt::ISize(x) => *x += 1,
            GenInt::U8(x) => *x += 1,
            GenInt::U16(x) => *x += 1,
            GenInt::U32(x) => *x += 1,
            GenInt::U64(x) => *x += 1,
            GenInt::U128(x) => *x += 1,
            GenInt::USize(x) => *x += 1,
        }
    }
}

use lazy_static::lazy_static;
use std::collections::{HashMap, HashSet};

lazy_static! {
    static ref KNOWN_CHARS: HashSet<char> = {
        let m = [
            0xc0, 0xe0, 0xc1, 0xe1, 0xc2, 0xe2, 0xc3, 0xe3, 0xc4, 0xe4, 0xc5, 0xe5, 0xc6, 0xe6,
            0xc7, 0xe7, 0xc8, 0xe8, 0xc9, 0xe9, 0xca, 0xea, 0xcb, 0xeb, 0xcc, 0xec, 0xcd, 0xed,
            0xce, 0xee, 0xcf, 0xef, 0xd0, 0xf0, 0xd1, 0xf1, 0xd2, 0xf2, 0xd3, 0xf3, 0xd4, 0xf4,
            0xd5, 0xf5, 0xd6, 0xf6, 0xd8, 0xf8, 0xd9, 0xf9, 0xda, 0xfa, 0xdb, 0xfb, 0xdc, 0xfc,
            0xdd, 0xfd, 0xde, 0xfe, 0x160, 0x161, 0x17d, 0x17e, 0x152, 0x153, 0x178, 0xff, 0x1e9e,
            0xdf,
        ]
        .iter()
        .map(|x| char::from_u32(*x).unwrap())
        .collect();

        m
    };
    static ref KNOWN_PAIRS: HashMap<(char, char), char> = {
        let mut pairs = HashMap::new();
        pairs.insert(
            ('A', char::from_u32(0x300).unwrap()),
            char::from_u32(0xc0).unwrap(),
        );
        pairs.insert(
            ('A', char::from_u32(0x301).unwrap()),
            char::from_u32(0xc1).unwrap(),
        );
        pairs.insert(
            ('A', char::from_u32(0x302).unwrap()),
            char::from_u32(0xc2).unwrap(),
        );
        pairs.insert(
            ('A', char::from_u32(0x303).unwrap()),
            char::from_u32(0xc3).unwrap(),
        );
        pairs.insert(
            ('A', char::from_u32(0x308).unwrap()),
            char::from_u32(0xc4).unwrap(),
        );
        pairs.insert(
            ('A', char::from_u32(0x30a).unwrap()),
            char::from_u32(0xc5).unwrap(),
        );
        pairs.insert(
            ('C', char::from_u32(0x327).unwrap()),
            char::from_u32(0xc7).unwrap(),
        );
        pairs.insert(
            ('E', char::from_u32(0x300).unwrap()),
            char::from_u32(0xc8).unwrap(),
        );
        pairs.insert(
            ('E', char::from_u32(0x301).unwrap()),
            char::from_u32(0xc9).unwrap(),
        );
        pairs.insert(
            ('E', char::from_u32(0x302).unwrap()),
            char::from_u32(0xca).unwrap(),
        );
        pairs.insert(
            ('E', char::from_u32(0x308).unwrap()),
            char::from_u32(0xcb).unwrap(),
        );
        pairs.insert(
            ('I', char::from_u32(0x300).unwrap()),
            char::from_u32(0xcc).unwrap(),
        );
        pairs.insert(
            ('I', char::from_u32(0x301).unwrap()),
            char::from_u32(0xcd).unwrap(),
        );
        pairs.insert(
            ('I', char::from_u32(0x302).unwrap()),
            char::from_u32(0xce).unwrap(),
        );
        pairs.insert(
            ('I', char::from_u32(0x308).unwrap()),
            char::from_u32(0xcf).unwrap(),
        );
        pairs.insert(
            ('N', char::from_u32(0x303).unwrap()),
            char::from_u32(0xd1).unwrap(),
        );
        pairs.insert(
            ('O', char::from_u32(0x300).unwrap()),
            char::from_u32(0xd2).unwrap(),
        );
        pairs.insert(
            ('O', char::from_u32(0x301).unwrap()),
            char::from_u32(0xd3).unwrap(),
        );
        pairs.insert(
            ('O', char::from_u32(0x302).unwrap()),
            char::from_u32(0xd4).unwrap(),
        );
        pairs.insert(
            ('O', char::from_u32(0x303).unwrap()),
            char::from_u32(0xd5).unwrap(),
        );
        pairs.insert(
            ('O', char::from_u32(0x308).unwrap()),
            char::from_u32(0xd6).unwrap(),
        );
        pairs.insert(
            ('U', char::from_u32(0x300).unwrap()),
            char::from_u32(0xd9).unwrap(),
        );
        pairs.insert(
            ('U', char::from_u32(0x301).unwrap()),
            char::from_u32(0xda).unwrap(),
        );
        pairs.insert(
            ('U', char::from_u32(0x302).unwrap()),
            char::from_u32(0xdb).unwrap(),
        );
        pairs.insert(
            ('U', char::from_u32(0x308).unwrap()),
            char::from_u32(0xdc).unwrap(),
        );
        pairs.insert(
            ('Y', char::from_u32(0x301).unwrap()),
            char::from_u32(0xdd).unwrap(),
        );
        pairs.insert(
            ('Y', char::from_u32(0x308).unwrap()),
            char::from_u32(0x178).unwrap(),
        );
        pairs.insert(
            ('S', char::from_u32(0x30c).unwrap()),
            char::from_u32(0x160).unwrap(),
        );
        pairs.insert(
            ('Z', char::from_u32(0x30c).unwrap()),
            char::from_u32(0x17d).unwrap(),
        );
        pairs.insert(
            ('a', char::from_u32(0x300).unwrap()),
            char::from_u32(0xe0).unwrap(),
        );
        pairs.insert(
            ('a', char::from_u32(0x301).unwrap()),
            char::from_u32(0xe1).unwrap(),
        );
        pairs.insert(
            ('a', char::from_u32(0x302).unwrap()),
            char::from_u32(0xe2).unwrap(),
        );
        pairs.insert(
            ('a', char::from_u32(0x303).unwrap()),
            char::from_u32(0xe3).unwrap(),
        );
        pairs.insert(
            ('a', char::from_u32(0x308).unwrap()),
            char::from_u32(0xe4).unwrap(),
        );
        pairs.insert(
            ('a', char::from_u32(0x30a).unwrap()),
            char::from_u32(0xe5).unwrap(),
        );
        pairs.insert(
            ('c', char::from_u32(0x327).unwrap()),
            char::from_u32(0xe7).unwrap(),
        );
        pairs.insert(
            ('e', char::from_u32(0x300).unwrap()),
            char::from_u32(0xe8).unwrap(),
        );
        pairs.insert(
            ('e', char::from_u32(0x301).unwrap()),
            char::from_u32(0xe9).unwrap(),
        );
        pairs.insert(
            ('e', char::from_u32(0x302).unwrap()),
            char::from_u32(0xea).unwrap(),
        );
        pairs.insert(
            ('e', char::from_u32(0x308).unwrap()),
            char::from_u32(0xeb).unwrap(),
        );
        pairs.insert(
            ('i', char::from_u32(0x300).unwrap()),
            char::from_u32(0xec).unwrap(),
        );
        pairs.insert(
            ('i', char::from_u32(0x301).unwrap()),
            char::from_u32(0xed).unwrap(),
        );
        pairs.insert(
            ('i', char::from_u32(0x302).unwrap()),
            char::from_u32(0xee).unwrap(),
        );
        pairs.insert(
            ('i', char::from_u32(0x308).unwrap()),
            char::from_u32(0xef).unwrap(),
        );
        pairs.insert(
            ('n', char::from_u32(0x303).unwrap()),
            char::from_u32(0xf1).unwrap(),
        );
        pairs.insert(
            ('o', char::from_u32(0x300).unwrap()),
            char::from_u32(0xf2).unwrap(),
        );
        pairs.insert(
            ('o', char::from_u32(0x301).unwrap()),
            char::from_u32(0xf3).unwrap(),
        );
        pairs.insert(
            ('o', char::from_u32(0x302).unwrap()),
            char::from_u32(0xf4).unwrap(),
        );
        pairs.insert(
            ('o', char::from_u32(0x303).unwrap()),
            char::from_u32(0xf5).unwrap(),
        );
        pairs.insert(
            ('o', char::from_u32(0x308).unwrap()),
            char::from_u32(0xf6).unwrap(),
        );
        pairs.insert(
            ('u', char::from_u32(0x300).unwrap()),
            char::from_u32(0xf9).unwrap(),
        );
        pairs.insert(
            ('u', char::from_u32(0x301).unwrap()),
            char::from_u32(0xfa).unwrap(),
        );
        pairs.insert(
            ('u', char::from_u32(0x302).unwrap()),
            char::from_u32(0xfb).unwrap(),
        );
        pairs.insert(
            ('u', char::from_u32(0x308).unwrap()),
            char::from_u32(0xfc).unwrap(),
        );
        pairs.insert(
            ('y', char::from_u32(0x301).unwrap()),
            char::from_u32(0xfd).unwrap(),
        );
        pairs.insert(
            ('y', char::from_u32(0x308).unwrap()),
            char::from_u32(0xff).unwrap(),
        );
        pairs.insert(
            ('s', char::from_u32(0x30c).unwrap()),
            char::from_u32(0x161).unwrap(),
        );
        pairs.insert(
            ('z', char::from_u32(0x30c).unwrap()),
            char::from_u32(0x17e).unwrap(),
        );
        pairs
    };
}

// pub fn capitalize(s: &str) -> Result<String, String> {
//     Ok(s.chars().map(|x| x.to_uppercase()).flatten().collect())
// }

pub fn capitalize(s: &str) -> Result<String, String> {
    let mut first = true;
    if s.chars().any(|u| u.to_uppercase().len() != 1) {
        return Err("todo".into());
    }
    normalize_generic(
        false,
        |u| {
            if first {
                first = false;
                u.to_uppercase().next().unwrap()
            } else {
                u
            }
        },
        s,
    )
}

pub fn uncapitalize(s: &str) -> Result<String, String> {
    let mut first = true;
    if s.chars().any(|u| u.to_lowercase().len() != 1) {
        return Err("todo".into());
    }
    normalize_generic(
        false,
        |u| {
            if first {
                first = false;
                u.to_lowercase().next().unwrap()
            } else {
                u
            }
        },
        s,
    )
}

pub fn normalized_unit_filename(s: &str) -> Result<String, String> {
    uncapitalize(s)
}

pub fn char_not_identifier_start(c: char) -> bool {
    let c = c as u32;
    c >= 48 && c <= 57 || c == 39
}

pub fn char_valid_in_identifier(c: char, with_dot: bool) -> bool {
    let c2 = c as u32;
    if c2 < 0x80 {
        c2 >= 97 && c2 <= 122
            || c2 >= 65 && c2 <= 90
            || c2 >= 48 && c2 <= 57
            || c2 == 95
            || c2 == 39
            || (with_dot && c2 == 46)
    } else {
        KNOWN_CHARS.contains(&c)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValidationResult {
    Valid,
    Empty,
    InvalidCharacter(char),
    InvalidBeginning(char),
}

#[inline(always)]
pub fn validate_identifier(s: &str) -> ValidationResult {
    validate_identifier_pro(false, s)
}

pub fn validate_identifier_pro(with_dot: bool, s: &str) -> ValidationResult {
    let (first, rest) = match s.split_at_checked(1) {
        Some(x) => x,
        None => return ValidationResult::Empty,
    };

    let first = first.chars().nth(0).unwrap();
    if char_not_identifier_start(first) {
        ValidationResult::InvalidBeginning(first)
    } else if !rest.chars().all(|x| char_valid_in_identifier(x, with_dot)) {
        ValidationResult::InvalidCharacter(
            rest.chars()
                .find(|&c| !char_valid_in_identifier(c, with_dot))
                .unwrap(),
        )
    } else {
        ValidationResult::Valid
    }
}

pub fn is_valid_identifier(s: &str) -> bool {
    match validate_identifier(s) {
        ValidationResult::Valid => true,
        _ => false,
    }
}

pub fn normalize_generic<F>(keep_ascii: bool, mut transform: F, s: &str) -> Result<String, String>
where
    F: FnMut(char) -> char,
{
    if s.is_empty() {
        return Ok(s.to_string());
    }
    if keep_ascii && s.chars().all(|c| (c as u32) < 128) {
        return Ok(s.to_string());
    }

    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars();
    let mut valid = true;

    let mut prev = match chars.next() {
        Some(c) if c != '\u{FFFD}' => c,
        Some(_) => {
            valid = false;
            '\u{FFFD}'
        }
        None => return Ok(String::new()),
    };

    for curr in chars {
        if curr == '\u{FFFD}' {
            valid = false;
        }
        if let Some(&composed) = KNOWN_PAIRS.get(&(prev, curr)) {
            prev = composed;
        } else {
            result.push(transform(prev));
            prev = curr;
        }
    }
    result.push(transform(prev));

    if valid {
        Ok(result)
    } else {
        Err(result)
    }
}

pub fn normalize(s: &str) -> Result<String, String> {
    normalize_generic(true, |c| c, s)
}

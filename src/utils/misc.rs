use lazy_static::lazy_static;
use std::collections::HashSet;

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
}

pub fn capitalize(s: &str) -> Result<String, String> {
    Ok(s.chars().map(|x| x.to_uppercase()).flatten().collect())
}

pub fn normalized_unit_filename(s: &str) -> Result<String, String> {
    Ok(s.to_string())
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

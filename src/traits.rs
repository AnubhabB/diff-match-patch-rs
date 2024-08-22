use std::hash::Hash;

use chrono::NaiveTime;

use crate::dmp::{Diff, DiffMatchPatch};

pub trait DType: Copy + Ord + Eq + Hash {
    fn bisect_split(
        dmp: &DiffMatchPatch,
        old: &[Self],
        new: &[Self],
        x: usize,
        y: usize,
        start: NaiveTime,
    ) -> Result<Vec<Diff<Self>>, crate::errors::Error>;

    fn newline() -> Self;
    fn minus() -> Self;
    fn plus() -> Self;
    fn comma() -> Self;
    fn whitespace() -> Self;
    fn at() -> Self;
    fn is_whitespace(&self) -> bool;
    fn is_alphanumeric(&self) -> bool;
    fn is_linebreak(&self) -> bool;

    fn ends_with_newlines(d: &[Self]) -> bool;
    fn starts_with_newlines(d: &[Self]) -> bool;
    fn to_bytes(d: &[Self]) -> Vec<u8>;
    fn is_sep(d: &[Self]) -> bool;
    fn to_string(d: &[Self]) -> Result<String, crate::errors::Error>;
    fn from_str(d: &str) -> Vec<Self>;

    fn null_pad(len: u8) -> Vec<Self>;
}

impl DType for u8 {
    fn bisect_split(
        dmp: &DiffMatchPatch,
        old: &[u8],
        new: &[u8],
        x: usize,
        y: usize,
        start: NaiveTime,
    ) -> Result<Vec<Diff<u8>>, crate::errors::Error> {
        let old_a = &old[..x];
        let new_a = &new[..y];

        let old_b = &old[x..];
        let new_b = &new[y..];

        // Compute both diffs serially.
        let mut diffs_a = dmp.diff_internal(old_a, new_a, false, start)?;
        diffs_a.append(&mut dmp.diff_internal(old_b, new_b, false, start)?);

        Ok(diffs_a)
    }

    fn newline() -> Self {
        b'\n'
    }

    fn minus() -> Self {
        b'-'
    }

    fn plus() -> Self {
        b'+'
    }

    fn comma() -> Self {
        b','
    }

    fn whitespace() -> Self {
        b' '
    }

    fn at() -> Self {
        b'@'
    }

    fn is_whitespace(&self) -> bool {
        let d = *self as char;
        d.is_whitespace()
    }

    fn is_alphanumeric(&self) -> bool {
        let d = *self as char;
        d.is_alphanumeric()
    }

    fn is_linebreak(&self) -> bool {
        let d = *self as char;
        d.is_linebreak()
    }

    fn ends_with_newlines(d: &[Self]) -> bool {
        d.ends_with(b"\n\n") || d.ends_with(b"\n\r\n")
    }

    fn starts_with_newlines(d: &[Self]) -> bool {
        d.starts_with(b"\r\n\n")
        || d.starts_with(b"\r\n\r\n")
        || d.starts_with(b"\n\r\n")
        || d.starts_with(b"\n\n")
    }

    fn to_bytes(d: &[Self]) -> Vec<u8> {
        d.to_vec()
    }

    fn is_sep(d: &[Self]) -> bool {
        d == b"@@"
    }

    fn to_string(d: &[Self]) -> Result<String, crate::errors::Error> {
        match std::str::from_utf8(d) {
            Ok(s) => Ok(s.to_string()),
            Err(_) => Err(crate::errors::Error::Utf8Error)
        }
    }

    fn null_pad(len: u8) -> Vec<Self> {
        (1..len + 1).collect()
    }

    fn from_str(d: &str) -> Vec<Self> {
        d.as_bytes().to_vec()
    }
}

impl DType for char {
    fn bisect_split(
        dmp: &DiffMatchPatch,
        old: &[char],
        new: &[char],
        x: usize,
        y: usize,
        start: NaiveTime,
    ) -> Result<Vec<Diff<char>>, crate::errors::Error> {
        let old_a = &old[..x];
        let new_a = &new[..y];

        let old_b = &old[x..];
        let new_b = &new[y..];

        // Compute both diffs serially.
        let mut diffs_a = dmp.diff_internal(old_a, new_a, false, start)?;
        diffs_a.append(&mut dmp.diff_internal(old_b, new_b, false, start)?);

        Ok(diffs_a)
    }

    fn newline() -> Self {
        '\n'
    }

    fn minus() -> Self {
        '-'
    }

    fn plus() -> Self {
        '+'
    }

    fn comma() -> Self {
        ','
    }

    fn whitespace() -> Self {
        ' '
    }

    fn at() -> Self {
        '@'
    }

    fn is_whitespace(&self) -> bool {
        let d = *self;
        d.is_whitespace()
    }

    fn is_alphanumeric(&self) -> bool {
        let d = *self;
        d.is_alphanumeric()
    }

    fn is_linebreak(&self) -> bool {
        let c = *self;

        c == '\n' || c == '\r'
    }

    fn ends_with_newlines(d: &[Self]) -> bool {
        d.ends_with(&['\n', '\n']) || d.ends_with(&['\n', '\r', '\n'])
    }

    fn starts_with_newlines(d: &[Self]) -> bool {
        d.starts_with(&['\r', '\n', '\n'])
        || d.starts_with(&['\r', '\n', '\r', '\n'])
        || d.starts_with(&['\n', '\r', '\n'])
        || d.starts_with(&['\n', '\n'])
    }

    fn to_bytes(d: &[Self]) -> Vec<u8> {
        d.iter().collect::<String>().as_bytes().to_vec()
    }

    fn is_sep(d: &[Self]) -> bool {
        d == &['@', '@']
    }

    fn to_string(d: &[Self]) -> Result<String, crate::errors::Error> {
        Ok(d.iter().collect::<String>())
    }

    fn null_pad(len: u8) -> Vec<Self> {
        (1..len + 1)
            .map(|c| c as char)
            .collect::<Vec<_>>()
    }

    fn from_str(d: &str) -> Vec<Self> {
        d.chars().collect::<Vec<_>>()
    }
}

impl DType for u32 {
    fn bisect_split(
        dmp: &DiffMatchPatch,
        old: &[u32],
        new: &[u32],
        x: usize,
        y: usize,
        start: NaiveTime,
    ) -> Result<Vec<Diff<u32>>, crate::errors::Error> {
        let old_a = &old[..x];
        let new_a = &new[..y];

        let old_b = &old[x..];
        let new_b = &new[y..];

        // Compute both diffs serially.
        let mut diffs_a = dmp.diff_lines(old_a, new_a, start)?;
        diffs_a.append(&mut dmp.diff_lines(old_b, new_b, start)?);

        Ok(diffs_a)
    }

    fn newline() -> Self {
        '\n' as u32
    }

    fn minus() -> Self {
        '-' as u32
    }

    fn plus() -> Self {
        '+' as u32
    }

    fn comma() -> Self {
        ',' as u32
    }

    fn whitespace() -> Self {
        ' ' as u32
    }

    fn at() -> Self {
        '@' as u32
    }

    fn is_whitespace(&self) -> bool {
        let c: Option<u8> = (*self).try_into().ok();
        c.map_or(false, |c| (c as char).is_whitespace())
    }

    fn is_alphanumeric(&self) -> bool {
        let c: Option<u8> = (*self).try_into().ok();
        c.map_or(false, |c| (c as char).is_alphanumeric())
    }

    fn is_linebreak(&self) -> bool {
        let c: Option<u8> = (*self).try_into().ok();

        c.map_or(false, |c| (c as char).is_linebreak())
    }

    fn ends_with_newlines(_: &[Self]) -> bool {
        unimplemented!()
    }

    fn starts_with_newlines(_: &[Self]) -> bool {
        unimplemented!()   
    }

    fn to_bytes(_: &[Self]) -> Vec<u8> {
        unimplemented!()
    }

    fn is_sep(_: &[Self]) -> bool {
        unimplemented!()
    }

    fn to_string(_: &[Self]) -> Result<String, crate::errors::Error> {
        unimplemented!()
    }

    fn null_pad(_: u8) -> Vec<Self> {
        unimplemented!()
    }

    fn from_str(d: &str) -> Vec<Self> {
        // let t = d.chars().map(|c| c.int);
        d.chars().map(|c| {
            let d: u32 = c.into();
            d
        })
        .collect::<Vec<_>>()
    }
}

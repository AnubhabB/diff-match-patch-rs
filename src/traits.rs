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
        deadline: Option<NaiveTime>,
    ) -> Result<Vec<Diff<Self>>, crate::errors::Error>;

    fn from_char(c: char) -> Self;

    fn as_char(&self) -> Option<char>;

    fn from_str(str: &str) -> Vec<Self>;

    fn is_linebreak_end(input: &[Self]) -> bool;
    fn is_linebreak_start(input: &[Self]) -> bool;
}

impl DType for u8 {
    fn bisect_split(
        dmp: &DiffMatchPatch,
        old: &[u8],
        new: &[u8],
        x: usize,
        y: usize,
        deadline: Option<NaiveTime>,
    ) -> Result<Vec<Diff<u8>>, crate::errors::Error> {
        let old_a = &old[..x];
        let new_a = &new[..y];

        let old_b = &old[x..];
        let new_b = &new[y..];

        // Compute both diffs serially.
        let mut diffs_a = dmp.diff_internal(old_a, new_a, false, deadline)?;
        diffs_a.append(&mut dmp.diff_internal(old_b, new_b, false, deadline)?);

        Ok(diffs_a)
    }

    fn from_char(c: char) -> Self {
        c as u8
    }

    fn as_char(&self) -> Option<char> {
        Some(*self as char)
    }

    fn from_str(str: &str) -> Vec<Self> {
        str.as_bytes().to_vec()
    }

    #[inline]
    fn is_linebreak_end(input: &[Self]) -> bool {
        input.ends_with(b"\n\n") || input.ends_with(b"\n\r\n")
    }

    #[inline]
    fn is_linebreak_start(input: &[Self]) -> bool {
        input.starts_with(b"\r\n\n")
            || input.starts_with(b"\r\n\r\n")
            || input.starts_with(b"\n\r\n")
            || input.starts_with(b"\n\n")
    }
}

impl DType for char {
    fn bisect_split(
        dmp: &DiffMatchPatch,
        old: &[char],
        new: &[char],
        x: usize,
        y: usize,
        deadline: Option<NaiveTime>,
    ) -> Result<Vec<Diff<char>>, crate::errors::Error> {
        let old_a = &old[..x];
        let new_a = &new[..y];

        let old_b = &old[x..];
        let new_b = &new[y..];

        // Compute both diffs serially.
        let mut diffs_a = dmp.diff_internal(old_a, new_a, false, deadline)?;
        diffs_a.append(&mut dmp.diff_internal(old_b, new_b, false, deadline)?);

        Ok(diffs_a)
    }

    fn from_char(c: char) -> Self {
        c
    }

    fn as_char(&self) -> Option<char> {
        Some(*self)
    }

    fn from_str(str: &str) ->  Vec<Self> {
        str.chars().collect::<Vec<_>>()
    }

    #[inline]
    fn is_linebreak_end(input: &[Self]) -> bool {
        input.ends_with(&['\n', '\n']) || input.ends_with(&['\n', '\r', '\n'])
    }

    #[inline]
    fn is_linebreak_start(input: &[Self]) -> bool {
        input.starts_with(&['\r', '\n', '\n'])
            || input.starts_with(&['\r', '\n', '\r', '\n'])
            || input.starts_with(&['\n', '\r', '\n'])
            || input.starts_with(&['\n', '\n'])
    }
}

impl DType for usize {
    fn bisect_split(
        dmp: &DiffMatchPatch,
        old: &[usize],
        new: &[usize],
        x: usize,
        y: usize,
        deadline: Option<NaiveTime>,
    ) -> Result<Vec<Diff<usize>>, crate::errors::Error> {
        let old_a = &old[..x];
        let new_a = &new[..y];

        let old_b = &old[x..];
        let new_b = &new[y..];

        // Compute both diffs serially.
        let mut diffs_a = dmp.diff_lines(old_a, new_a, deadline)?;
        diffs_a.append(&mut dmp.diff_lines(old_b, new_b, deadline)?);

        Ok(diffs_a)
    }

    fn from_char(c: char) -> Self {
        (c as u8) as usize
    }

    fn as_char(&self) -> Option<char> {
        char::from_digit(*self as u32, 10)
    }

    fn from_str(_: &str) -> Vec<Self> {
        unimplemented!()
    }

    fn is_linebreak_end(_: &[Self]) -> bool {
        unimplemented!()
    }

    #[inline]
    fn is_linebreak_start(_: &[Self]) -> bool {
        unimplemented!()
    }
}
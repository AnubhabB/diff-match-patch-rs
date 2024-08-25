use std::hash::Hash;

use chrono::NaiveTime;
use percent_encoding::{AsciiSet, CONTROLS};

use crate::dmp::{Diff, DiffMatchPatch};

// Appending controls to ensure exact same encoding as cpp variant
const ENCODE_SET: &AsciiSet = &CONTROLS
    .add(b'"')
    .add(b'<')
    .add(b'>')
    .add(b'`')
    .add(b'{')
    .add(b'}')
    .add(b'%')
    .add(b'[')
    .add(b'\\')
    .add(b']')
    .add(b'^')
    .add(b'|');


pub trait DType: Copy + Ord + Eq + Hash {
    fn differ(dmp: &DiffMatchPatch, txt_old: &str, txt_new: &str) -> Result<Vec<Diff<Self>>, crate::errors::Error>;
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
    fn to_string(data: &[Self]) -> Result<String, crate::Error>;

    fn is_linebreak_end(input: &[Self]) -> bool;
    fn is_linebreak_start(input: &[Self]) -> bool;

    fn percent_encode(input: &[Self]) -> Vec<Self>;
    fn percent_decode(input: &[Self]) -> Vec<Self>;
}

impl DType for u8 {
    fn differ(dmp: &DiffMatchPatch, txt_old: &str, txt_new: &str) -> Result<Vec<Diff<Self>>, crate::errors::Error> {
        dmp.diff_main(txt_old, txt_new)
    }

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
    fn to_string(data: &[Self]) -> Result<String, crate::Error> {
        std::str::from_utf8(data).map_err(|_| crate::Error::Utf8Error).map(|s| s.to_string())
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

    #[inline]
    fn percent_encode(input: &[Self]) -> Vec<Self> {
        percent_encoding::percent_encode(input, ENCODE_SET).collect::<String>().as_bytes().to_vec()
    }

    #[inline]
    fn percent_decode(input: &[Self]) -> Vec<Self> {
        // percent_encoding::percent_encode(input, ENCODE_SET).collect::<String>().as_bytes().to_vec()
        todo!()
    }
}

impl DType for char {
    fn differ(dmp: &DiffMatchPatch, txt_old: &str, txt_new: &str) -> Result<Vec<Diff<Self>>, crate::errors::Error> {
        dmp.diff_main_compat(txt_old, txt_new)
    }

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
    fn to_string(data: &[Self]) -> Result<String, crate::Error> {
        Ok(data.iter().collect::<String>())
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

    #[inline]
    fn percent_encode(input: &[Self]) -> Vec<Self> {
        let d = input
            .iter()
            .map(|c| {
                let mut b = vec![0; c.len_utf8()];
                c.encode_utf8(&mut b);
                
                b
            }).collect::<Vec<_>>()
            .concat();


        let encoded = percent_encoding::percent_encode(&d[..], ENCODE_SET).collect::<String>();

        Self::from_str(&encoded)
    }

    #[inline]
    fn percent_decode(input: &[Self]) -> Vec<Self> {
        todo!()
    }
}

impl DType for usize {
    fn differ(_: &DiffMatchPatch, _: &str, _: &str) -> Result<Vec<Diff<Self>>, crate::errors::Error> {
        unimplemented!()
    }

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

    fn to_string(_: &[Self]) -> Result<String, crate::Error> {
        unimplemented!()
    }

    fn is_linebreak_end(_: &[Self]) -> bool {
        unimplemented!()
    }

    #[inline]
    fn is_linebreak_start(_: &[Self]) -> bool {
        unimplemented!()
    }

    #[inline]
    fn percent_encode(_: &[Self]) -> Vec<Self> {
        unimplemented!()
    }

    #[inline]
    fn percent_decode(_: &[Self]) -> Vec<Self> {
        unimplemented!()
    }
}
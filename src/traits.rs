use std::hash::Hash;

use percent_encoding::{percent_decode, AsciiSet, CONTROLS};

use crate::{
    dmp::{Diff, DiffMatchPatch, Time},
    Ops,
};

pub type Efficient = u8;
pub type Compat = char;

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
    // fn differ(dmp: &DiffMatchPatch, txt_old: &str, txt_new: &str) -> Result<Vec<Diff<Self>>, crate::errors::Error>;
    fn bisect_split(
        dmp: &DiffMatchPatch,
        old: &[Self],
        new: &[Self],
        x: usize,
        y: usize,
        deadline: Option<Time>,
    ) -> Result<Vec<Diff<Self>>, crate::errors::Error>;

    fn from_char(c: char) -> Self;
    fn as_char(&self) -> Option<char>;
    fn from_str(str: &str) -> Vec<Self>;
    fn to_string(data: &[Self]) -> Result<String, crate::Error>;

    fn is_linebreak_end(input: &[Self]) -> bool;
    fn is_linebreak_start(input: &[Self]) -> bool;

    fn percent_encode(input: &[Self]) -> Vec<Self>;
    fn percent_decode(input: &[Self]) -> Vec<Self>;

    fn humanize(_diffs: &mut Vec<Diff<Self>>) -> Result<(), crate::Error> {
        Ok(())
    }
}

impl DType for u8 {
    fn bisect_split(
        dmp: &DiffMatchPatch,
        old: &[u8],
        new: &[u8],
        x: usize,
        y: usize,
        deadline: Option<Time>,
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
        std::str::from_utf8(data)
            .map_err(|_| crate::Error::Utf8Error)
            .map(|s| s.to_string())
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
        percent_encoding::percent_encode(input, ENCODE_SET)
            .collect::<String>()
            .as_bytes()
            .to_vec()
    }

    #[inline]
    fn percent_decode(input: &[Self]) -> Vec<Self> {
        percent_decode(input).collect()
    }

    #[inline]
    fn humanize(diffs: &mut Vec<Diff<Self>>) -> Result<(), crate::Error> {
        let mut idx = 0_usize;
        let mut err_prefix = vec![];

        let mut err_start = None;

        // First pass, we'll chomp of errors in the diffs?
        // The pattern we have seen is that
        while idx < diffs.len() {
            let diff = &mut diffs[idx];

            if let Err(e) = std::str::from_utf8(diff.data()) {
                // Errors can come in 2 forms
                // 1. error at the end of bytes - we'll keep prefixing the error bytes to all non equalities that follow
                // 2. error at the begining of bytes - this one is tricky - we'll need to figure out the suffix at which the rest of the string is valid
                if e.error_len().is_none() && err_start.is_none() {
                    err_start = Some(idx);

                    if diff.op() == Ops::Equal {
                        err_prefix = diff.data()[e.valid_up_to()..].to_vec();
                        diff.1 = if e.valid_up_to() > 0 {
                            diff.data()[..e.valid_up_to()].to_vec()
                        } else {
                            vec![]
                        };

                        idx += 1;
                        continue;
                    }
                }

                if let Some(err_start_idx) = err_start {
                    // For insert and delete add the prefix collected earlier (end error bytes)
                    if diff.op() == Ops::Delete || diff.op() == Ops::Insert {
                        diff.1 = [&err_prefix, diff.data()].concat();
                    } else {
                        if let Some(err_len) = e.error_len() {
                            // Iteratively figure out at what point does the error go away if at-all
                            let mut suffix = diff.data()[..err_len].to_vec();
                            let mut data = diff.data()[err_len..].to_vec();

                            while let Err(e) = std::str::from_utf8(&data) {
                                if e.error_len().is_none() {
                                    break;
                                }

                                // should never panic cos empty data is also a valid utf8
                                let first_byte = data.remove(0);
                                suffix.push(first_byte);
                            }

                            // here, we have a suffix to be added to all previous cases and a data that might be good string or error at the end of bytes
                            // which is a separate cycle

                            // Let's add the suffix to all the intermediate steps
                            diff.1 = data.to_vec();
                            diffs
                                .iter_mut()
                                .take(idx)
                                .skip(err_start_idx)
                                .for_each(|d| {
                                    if d.op() == Ops::Equal {
                                        return;
                                    }
                                    d.1 = [d.data(), &suffix[..]].concat();
                                });

                            // An equality within edits, lets seek the next one and update this suffix too
                            if data.is_empty() {
                                if idx < diffs.len() - 1 && diffs[idx + 1].op() != Ops::Equal {
                                    diffs[idx + 1].1 =
                                        [&err_prefix[..], &suffix, diffs[idx + 1].data()].concat();
                                }

                                diffs.remove(idx);
                            }
                        }

                        // Move back to where all of this started
                        idx = err_start_idx;
                        err_start = None;
                        err_prefix = vec![];
                        continue;
                    }
                }
            }
            idx += 1;
        }

        Ok(())
    }
}

impl DType for char {
    fn bisect_split(
        dmp: &DiffMatchPatch,
        old: &[char],
        new: &[char],
        x: usize,
        y: usize,
        deadline: Option<Time>,
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

    fn from_str(str: &str) -> Vec<Self> {
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
            })
            .collect::<Vec<_>>()
            .concat();

        let encoded = percent_encoding::percent_encode(&d[..], ENCODE_SET).collect::<String>();

        Self::from_str(&encoded)
    }

    #[inline]
    fn percent_decode(input: &[Self]) -> Vec<Self> {
        let ip = input.iter().collect::<String>();
        percent_decode(ip.as_bytes())
            .decode_utf8()
            .unwrap()
            .chars()
            .collect()
    }
}

impl DType for usize {
    fn bisect_split(
        dmp: &DiffMatchPatch,
        old: &[usize],
        new: &[usize],
        x: usize,
        y: usize,
        deadline: Option<Time>,
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

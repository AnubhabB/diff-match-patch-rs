use core::str;
use std::{char, collections::HashMap, fmt::Display};

use chrono::{NaiveTime, TimeDelta, Utc};
use percent_encoding::{percent_decode, percent_encode, CONTROLS};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
#[cfg(feature = "serde")]
use serde_repr::{Deserialize_repr, Serialize_repr};

use crate::{errors::Error, traits::BisectSplit};

/// Enum representing the different ops of diff
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize_repr, Deserialize_repr))]
#[repr(i8)]
pub enum Ops {
    Delete = -1,
    Insert,
    Equal,
}

/// A structure representing a diff
/// (Ops::Delete, String::new("Hello")) means delete `Hello`
/// (Ops::Insert, String::new("Goodbye")) means add `Goodbye`
/// (Ops::Equal, String::new("World")) means keep world
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Diff<T: Copy + Ord + Eq>(Ops, Vec<T>);

impl<T: Copy + Ord + Eq> Diff<T> {
    /// Create a new diff object
    pub fn new(op: Ops, data: &[T]) -> Self {
        Self(op, data.to_vec())
    }

    /// helper functions to create ops
    pub fn delete(data: &[T]) -> Self {
        Self::new(Ops::Delete, data)
    }

    pub fn insert(data: &[T]) -> Self {
        Self::new(Ops::Insert, data)
    }

    pub fn equal(data: &[T]) -> Self {
        Self::new(Ops::Equal, data)
    }

    // returns the operation of the current diff
    pub fn op(&self) -> Ops {
        self.0
    }

    // returns the bytes of the data
    pub fn data(&self) -> &[T] {
        &self.1[..]
    }

    // returns length of data
    pub fn size(&self) -> usize {
        self.1.len()
    }
}

pub struct DiffMatchPatch {
    /// a speedup flag, If present and false, then don't run
    /// a line-level diff first to identify the changed areas.
    /// Defaults to true, which does a faster, slightly less optimal diff.
    checklines: bool,
    /// A default timeout in num milliseconda, defaults to 1000 (1 second)
    timeout: Option<u32>,
    /// At what point is no match declared (0.0 = perfection, 1.0 = very loose).
    match_threshold: f32,
    /// How far to search for a match (0 = exact location, 1000+ = broad match).
    /// A match this many characters away from the expected location will add
    /// 1.0 to the score (0.0 is a perfect match).
    /// int Match_Distance;
    match_distance: usize,
    /// When deleting a large block of text (over ~64 characters), how close does
    /// the contents have to match the expected contents. (0.0 = perfection,
    /// 1.0 = very loose).  Note that Match_Threshold controls how closely the
    /// end points of a delete need to match.
    delete_threshold: f32,
    /// Chunk size for context length.
    patch_margin: u16,
    /// The number of bits in an int.
    match_max_bits: usize,
}

impl Default for DiffMatchPatch {
    fn default() -> Self {
        Self {
            checklines: true,
            timeout: Some(1000),
            match_threshold: 0.5,
            match_distance: 1000,
            match_max_bits: 32,
            patch_margin: 4,
            delete_threshold: 0.5,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct HalfMatch<'a, T: Copy + Ord + Eq> {
    prefix_long: &'a [T],
    suffix_long: &'a [T],
    prefix_short: &'a [T],
    suffix_short: &'a [T],
    common: &'a [T],
}

impl DiffMatchPatch {
    fn checklines(&self) -> bool {
        self.checklines
    }

    pub fn set_chechlines(&mut self, checklines: bool) {
        self.checklines = checklines;
    }

    // returns the configured timeout, defaults to `1`, None or `0` would mean infinite timeout
    fn timeout(&self) -> Option<i64> {
        self.timeout.map(|t| t as i64)
    }

    pub fn set_timeout(&mut self, tout: Option<u32>) {
        self.timeout = tout;
    }

    // creates a deadline from the given timeout
    fn deadline(&self) -> Option<NaiveTime> {
        self.timeout()
            .and_then(|t| Utc::now().checked_add_signed(TimeDelta::milliseconds(t)))
            .map(|t| t.time())
    }

    // returns configured match_threshold
    fn match_threshold(&self) -> f32 {
        self.match_threshold
    }

    pub fn set_match_threshold(&mut self, threshold: f32) {
        self.match_threshold = threshold
    }

    // returns the current patch margin
    fn patch_margin(&self) -> u16 {
        self.patch_margin
    }

    // returns the configured patch delete threshold
    fn delete_threshold(&self) -> f32 {
        self.delete_threshold
    }

    // returns the configured max_bits
    fn match_max_bits(&self) -> usize {
        self.match_max_bits
    }

    fn match_distance(&self) -> usize {
        self.match_distance
    }

    pub(crate) fn diff_internal<'a>(
        &self,
        old_bytes: &'a [u8],
        new_bytes: &'a [u8],
        linemode: bool,
        deadline: Option<NaiveTime>,
    ) -> Result<Vec<Diff<u8>>, crate::errors::Error> {
        // First, check if lhs and rhs are equal
        if old_bytes == new_bytes {
            if old_bytes.is_empty() {
                return Ok(Vec::new());
            }

            return Ok(vec![Diff::equal(old_bytes)]);
        }

        if old_bytes.is_empty() {
            return Ok(vec![Diff::insert(new_bytes)]);
        }

        if new_bytes.is_empty() {
            return Ok(vec![Diff::delete(old_bytes)]);
        }

        // Trim common prefix
        let common_prefix = Self::common_prefix(old_bytes, new_bytes, false);
        let common_suffix = Self::common_prefix(
            &old_bytes[common_prefix..],
            &new_bytes[common_prefix..],
            true,
        );

        let mut diffs = self.compute(
            &old_bytes[common_prefix..old_bytes.len() - common_suffix],
            &new_bytes[common_prefix..new_bytes.len() - common_suffix],
            linemode,
            deadline,
        )?;

        // Restore the prefix and suffix.
        if common_prefix > 0 {
            let mut d = vec![Diff::equal(&old_bytes[..common_prefix])];
            d.append(&mut diffs);
            diffs = d;
        }

        if common_suffix > 0 {
            diffs.push(Diff::new(
                Ops::Equal,
                &new_bytes[new_bytes.len() - common_suffix..],
            ));
        }

        Self::cleanup_merge(&mut diffs);

        Ok(diffs)
    }

    fn compute<'a>(
        &self,
        old: &'a [u8],
        new: &'a [u8],
        linemode: bool,
        deadline: Option<NaiveTime>,
    ) -> Result<Vec<Diff<u8>>, crate::errors::Error> {
        // returning all of the new part
        if old.is_empty() {
            return Ok(vec![Diff::insert(new)]);
        }

        // return everything deleted
        if new.is_empty() {
            return Ok(vec![Diff::delete(old)]);
        }

        let (long, short, old_gt_new) = if old.len() > new.len() {
            (old, new, true)
        } else {
            (new, old, false)
        };

        // found a subsequence which contains the short text
        if let Some(idx) = long
            .windows(short.len())
            .step_by(1)
            .position(|k| k == short)
        {
            // Shorter text is inside the longer text (speedup).
            let op = if old_gt_new { Ops::Delete } else { Ops::Insert };
            let diffs = vec![
                Diff::new(op, &long[0..idx]),
                Diff::equal(short),
                Diff::new(op, &long[idx + short.len()..]),
            ];

            return Ok(diffs);
        }

        if short.len() == 1 {
            // After previous case, this can't be an equality
            return Ok(vec![Diff::delete(old), Diff::insert(new)]);
        }

        // Check if the problem can be split in two
        if let Some(half_match) = self.half_match(old, new) {
            let old_a = half_match.prefix_long;
            let old_b = half_match.suffix_long;

            let new_a = half_match.prefix_short;
            let new_b = half_match.suffix_short;

            let mid_common = half_match.common;

            // Send both pairs off for separate processing.
            let mut diffs_a = match self.diff_internal(old_a, new_a, linemode, deadline) {
                Ok(d) => d,
                Err(_) => return Err(crate::errors::Error::Utf8Error),
            };
            let mut diffs_b = match self.diff_internal(old_b, new_b, linemode, deadline) {
                Ok(d) => d,
                Err(_) => return Err(crate::errors::Error::Utf8Error),
            };

            // Merge the results
            diffs_a.push(Diff::equal(mid_common));
            diffs_a.append(&mut diffs_b);

            return Ok(diffs_a);
        }

        if linemode && old.len() > 100 && new.len() > 100 {
            return self.line_mode(old, new, deadline);
        }

        match self.bisect(old, new, deadline) {
            Ok(b) => Ok(b),
            Err(_) => Err(crate::errors::Error::Utf8Error),
        }
    }

    fn half_match<'a, T: Copy + Ord + Eq>(
        &self,
        old: &'a [T],
        new: &'a [T],
    ) -> Option<HalfMatch<'a, T>> {
        // Don't risk returning a suboptimal diff when we have unlimited time
        self.timeout()?;

        let (long, short) = if old.len() > new.len() {
            (old, new)
        } else {
            (new, old)
        };

        // pointless - two small for this algo
        if long.len() < 4 || short.len() * 2 < long.len() {
            return None;
        }

        // First check if the second quarter is the seed for a half-match.
        // let hm1 = Self::diff_half_match_i(long, short, (long.len() as f32 / 4.).ceil() as usize);
        let hm1 = Self::half_match_i(long, short, long.len() / 4);
        // Check again based on the third quarter.
        // let hm2 = Self::diff_half_match_i(long, short, (long.len() as f32 / 2.).ceil() as usize);
        let hm2 = Self::half_match_i(long, short, long.len() / 2);

        if hm1.is_none() && hm2.is_none() {
            return None;
        }

        let hm = if let (Some(hm1), None) = (&hm1, &hm2) {
            hm1
        } else if let (None, Some(hm2)) = (&hm1, &hm2) {
            hm2
        } else if let (Some(hm1), Some(hm2)) = (&hm1, &hm2) {
            // both match, select the longest
            if hm1.common.len() > hm2.common.len() {
                hm1
            } else {
                hm2
            }
        } else {
            return None;
        };

        // A half-match was found, sort out the return data.
        let half_match = if old.len() > new.len() {
            HalfMatch {
                prefix_long: hm.prefix_long,
                suffix_long: hm.suffix_long,
                prefix_short: hm.prefix_short,
                suffix_short: hm.suffix_short,
                common: hm.common,
            }
        } else {
            HalfMatch {
                prefix_long: hm.prefix_short,
                suffix_long: hm.suffix_short,
                prefix_short: hm.prefix_long,
                suffix_short: hm.suffix_long,
                common: hm.common,
            }
        };

        Some(half_match)
    }

    // Quick line-level diff on both strings, then rediff the parts for greater accuracy
    // This speedup can produce non-minimal diffs
    fn line_mode<'a>(
        &self,
        old: &'a [u8],
        new: &'a [u8],
        deadline: Option<NaiveTime>,
    ) -> Result<Vec<Diff<u8>>, crate::errors::Error> {
        let mut diffs = {
            let to_chars = Self::lines_to_chars(old, new);
            let diffs =
                self.diff_lines(&to_chars.chars_old[..], &to_chars.chars_new[..], deadline)?;
            // Convert diffs back to text
            Self::chars_to_lines(&diffs[..], &to_chars.lines[..])
        };

        // Eliminate freak matches
        Self::cleanup_semantic(&mut diffs);

        // Rediff any replacement blocks, this time character-by-character.

        // Add a dummy entry at the end.
        diffs.push(Diff::equal(&[]));
        let mut difflen = diffs.len();
        let mut pointer = 0_usize;

        // count of bytes inserted
        let mut insert_n = 0_usize;
        // count of bytes to delete
        let mut delete_n = 0_usize;
        // a temp holder for consequetive data being inserted
        let mut insert_data = Vec::new();
        // same for delete
        let mut delete_data = Vec::new();

        while pointer < difflen {
            match diffs[pointer].op() {
                Ops::Insert => {
                    insert_n += 1;
                    insert_data = [&insert_data[..], diffs[pointer].data()].concat();
                }
                Ops::Delete => {
                    delete_n += 1;
                    delete_data = [&delete_data[..], diffs[pointer].data()].concat();
                }
                Ops::Equal => {
                    // Upon reaching an equality, check for prior redundancies.
                    if delete_n >= 1 && insert_n >= 1 {
                        // Delete the offending records and add the merged ones.
                        let idxstart = pointer - delete_n - insert_n;
                        let idxend = idxstart + delete_n + insert_n;

                        diffs.drain(idxstart..idxend);

                        pointer = idxstart;

                        let mut subdiffs =
                            self.diff_internal(&delete_data, &insert_data, false, deadline)?;
                        let subdifflen = subdiffs.len();
                        subdiffs.drain(..).rev().for_each(|d| {
                            diffs.insert(pointer, d);
                        });

                        pointer += subdifflen;
                        difflen = diffs.len();
                    }
                    // resetting counters
                    insert_n = 0;
                    delete_n = 0;
                    delete_data = Vec::new();
                    insert_data = Vec::new();
                }
            }

            pointer += 1;
        }

        diffs.pop();

        Ok(diffs)
    }

    pub(crate) fn diff_lines<'a>(
        &self,
        old: &'a [usize],
        new: &'a [usize],
        deadline: Option<NaiveTime>,
    ) -> Result<Vec<Diff<usize>>, crate::errors::Error> {
        if old == new {
            if old.is_empty() {
                return Ok(Vec::new());
            }

            return Ok(vec![Diff::equal(old)]);
        }

        if old.is_empty() {
            return Ok(vec![Diff::insert(new)]);
        }

        if new.is_empty() {
            return Ok(vec![Diff::delete(old)]);
        }

        // Trim common prefix
        let common_prefix = Self::common_prefix(old, new, false);
        let common_suffix = Self::common_prefix(&old[common_prefix..], &new[common_prefix..], true);

        let mut diffs = self.compute_lines(
            &old[common_prefix..old.len() - common_suffix],
            &new[common_prefix..new.len() - common_suffix],
            deadline,
        )?;

        if common_prefix > 0 {
            diffs.insert(0, Diff::equal(&old[..common_prefix]));
        }

        if common_suffix > 0 {
            diffs.push(Diff::new(Ops::Equal, &new[new.len() - common_suffix..]));
        }

        Self::cleanup_merge(&mut diffs);

        Ok(diffs)
    }

    fn compute_lines<'a>(
        &self,
        old: &'a [usize],
        new: &'a [usize],
        deadline: Option<NaiveTime>,
    ) -> Result<Vec<Diff<usize>>, crate::errors::Error> {
        // returning all of the new part
        if old.is_empty() {
            return Ok(vec![Diff::insert(new)]);
        }

        // return everything deleted
        if new.is_empty() {
            return Ok(vec![Diff::delete(old)]);
        }

        let (long, short, old_gt_new) = if old.len() > new.len() {
            (old, new, true)
        } else {
            (new, old, false)
        };

        // found a subsequence which contains the short text
        if let Some(idx) = long
            .windows(short.len())
            .step_by(1)
            .position(|k| k == short)
        {
            // Shorter text is inside the longer text (speedup).
            let op = if old_gt_new { Ops::Delete } else { Ops::Insert };
            let diffs = vec![
                Diff::new(op, &long[0..idx]),
                Diff::equal(short),
                Diff::new(op, &long[idx + short.len()..]),
            ];

            return Ok(diffs);
        }

        if short.len() == 1 {
            // After previous case, this can't be an equality
            return Ok(vec![Diff::delete(old), Diff::insert(new)]);
        }

        // Check if the problem can be split in two
        if let Some(half_match) = self.half_match(old, new) {
            let old_a = half_match.prefix_long;
            let old_b = half_match.suffix_long;

            let new_a = half_match.prefix_short;
            let new_b = half_match.suffix_short;

            let mid_common = half_match.common;

            // Send both pairs off for separate processing.
            let mut diffs_a = match self.diff_lines(old_a, new_a, deadline) {
                Ok(d) => d,
                Err(_) => return Err(crate::errors::Error::Utf8Error),
            };
            let mut diffs_b = match self.diff_lines(old_b, new_b, deadline) {
                Ok(d) => d,
                Err(_) => return Err(crate::errors::Error::Utf8Error),
            };

            // Merge the results
            diffs_a.push(Diff::equal(mid_common));
            diffs_a.append(&mut diffs_b);

            return Ok(diffs_a);
        }

        match self.bisect(old, new, deadline) {
            Ok(b) => Ok(b),
            Err(_) => Err(crate::errors::Error::Utf8Error),
        }
    }

    // Find the 'middle snake' of a diff, split the problem in two
    // and return the recursively constructed diff.
    // See Myers 1986 paper: An O(ND) Difference Algorithm and Its Variations.
    pub fn bisect<'a, T: BisectSplit>(
        &self,
        old: &'a [T],
        new: &'a [T],
        deadline: Option<NaiveTime>,
    ) -> Result<Vec<Diff<T>>, crate::errors::Error> {
        // let text1_length = old.len() as isize;
        // let text2_length = new.len() as isize;
        let old_len = old.len() as isize;
        let new_len = new.len() as isize;

        let max_d = (old_len + new_len + 1) / 2;
        let v_offset = max_d;
        let v_len = 2 * max_d;

        let mut v1 = vec![-1_isize; v_len as usize];
        let mut v2 = vec![-1_isize; v_len as usize];

        v1[v_offset as usize + 1] = 0;
        v2[v_offset as usize + 1] = 0;

        let delta = old_len - new_len;
        // If the total number of characters is odd, then the front path will collide
        // with the reverse path.
        let front = delta % 2 != 0;

        // Offsets for start and end of k loop.
        // Prevents mapping of space beyond the grid.
        let mut k1start: isize = 0;
        let mut k1end: isize = 0;
        let mut k2start: isize = 0;
        let mut k2end: isize = 0;

        for d in 0..max_d {
            // Bail out if deadline is reached.
            if let Some(tout) = deadline {
                if Utc::now().time() > tout {
                    break;
                }
            }

            // Walk the front path one step.
            let mut k1 = -d + k1start;
            while k1 < d + 1 - k1end {
                let (x1, y1) = {
                    let k1_offset = v_offset + k1;
                    let mut x1 = if k1 == -d
                        || (k1 != d && v1[k1_offset as usize - 1] < v1[k1_offset as usize + 1])
                    {
                        v1[k1_offset as usize + 1]
                    } else {
                        v1[k1_offset as usize - 1] + 1
                    };

                    let mut y1 = x1 - k1;
                    while x1 < old_len && y1 < new_len {
                        let i1 = if x1 < 0 { old_len + x1 } else { x1 };
                        let i2 = if y1 < 0 { new_len + y1 } else { y1 };
                        if old[i1 as usize] != new[i2 as usize] {
                            break;
                        }
                        x1 += 1;
                        y1 += 1;
                    }
                    v1[k1_offset as usize] = x1;

                    (x1, y1)
                };

                if x1 > old_len {
                    // Ran off the right of the graph.
                    k1end += 2;
                } else if y1 > new_len {
                    // Ran off the bottom of the graph.
                    k1start += 2;
                } else if front {
                    let k2_offset = v_offset + delta - k1;
                    if k2_offset >= 0 && k2_offset < v_len && v2[k2_offset as usize] != -1 {
                        // Mirror x2 onto top-left coordinate system.
                        let x2 = old_len - v2[k2_offset as usize];
                        if x1 >= x2 {
                            // Overlap detected.
                            return T::bisect_split(
                                self,
                                old,
                                new,
                                x1 as usize,
                                y1 as usize,
                                deadline,
                            );
                        }
                    }
                }
                k1 += 2;
            }

            // Walk the reverse path one step.
            let mut k2 = -d + k2start;
            while k2 < d + 1 - k2end {
                let (mut x2, y2) = {
                    let k2_offset = v_offset + k2;

                    let mut x2 = if k2 == -d
                        || (k2 != d && v2[k2_offset as usize - 1] < v2[k2_offset as usize + 1])
                    {
                        v2[k2_offset as usize + 1]
                    } else {
                        v2[k2_offset as usize - 1] + 1
                    };

                    let mut y2 = x2 - k2;
                    while x2 < old_len && y2 < new_len {
                        let i1 = if old_len - x2 > 0 {
                            old_len - x2 - 1
                        } else {
                            x2 + 1
                        };
                        let i2 = if new_len - y2 > 0 {
                            new_len - y2 - 1
                        } else {
                            y2 + 1
                        };
                        if old[i1 as usize] != new[i2 as usize] {
                            break;
                        }
                        x2 += 1;
                        y2 += 1;
                    }
                    v2[k2_offset as usize] = x2;

                    (x2, y2)
                };

                if x2 > old_len {
                    // Ran off the left of the graph.
                    k2end += 2;
                } else if y2 > new_len {
                    // Ran off the top of the graph.
                    k2start += 2;
                } else if !front {
                    let k1_offset = v_offset + delta - k2;
                    if k1_offset >= 0 && k1_offset < v_len && v1[k1_offset as usize] != -1 {
                        let x1 = v1[k1_offset as usize];
                        let y1 = v_offset + x1 - k1_offset;
                        // Mirror x2 onto top-left coordinate system.
                        x2 = old_len - x2;
                        if x1 >= x2 {
                            // Overlap detected.
                            return T::bisect_split(
                                self,
                                old,
                                new,
                                x1 as usize,
                                y1 as usize,
                                deadline,
                            );
                        }
                    }
                }
                k2 += 2;
            }
        }

        Ok(vec![Diff::delete(old), Diff::insert(new)])
    }

    // Does a substring of shorttext exist within longtext such that the substring
    // is at least half the length of longtext?
    //idx Start index of quarter length substring within longtext.
    #[inline]
    fn half_match_i<'a, T: Copy + Ord + Eq>(
        long: &'a [T],
        short: &'a [T],
        idx: usize,
    ) -> Option<HalfMatch<'a, T>> {
        // Start with a 1/4 length substring at position i as a seed.

        let seed = &long[idx..idx + long.len() / 4];
        let seedleen = seed.len();
        let mut j = 0;

        let mut best_common: &[T] = &[];
        let mut best_long_a: &[T] = &[];
        let mut best_long_b: &[T] = &[];
        let mut best_short_a: &[T] = &[];
        let mut best_short_b: &[T] = &[];

        while let Some(pos) = &short[j..]
            .windows(seedleen)
            .step_by(1)
            .position(|p| p == seed)
        {
            j += *pos;

            let prefix_len = Self::common_prefix(&long[idx..], &short[j..], false);
            let suffix_len = Self::common_prefix(&long[..idx], &short[..j], true);

            if best_common.len() < suffix_len + prefix_len {
                best_common = &short[j - suffix_len..j + prefix_len];

                best_long_a = &long[..idx - suffix_len];
                best_long_b = &long[idx + prefix_len..];

                best_short_a = &short[..j - suffix_len];
                best_short_b = &short[j + prefix_len..];
            }

            j += 1;
        }

        if best_common.len() * 2 >= long.len() {
            Some(HalfMatch {
                prefix_long: best_long_a,
                suffix_long: best_long_b,
                prefix_short: best_short_a,
                suffix_short: best_short_b,
                common: best_common,
            })
        } else {
            None
        }
    }

    // returns the number of bytes common in both the str - this is the position in bytes not chars, [0 .. n] is your prefix
    // We are doing a binary search here, and I've observed similar performance as noted by https://neil.fraser.name/news/2007/10/09/
    // Some benchmark code can be found in benches/prefix.rs
    // Reverse prefix is suffix
    // TODO: investigate this further
    #[inline]
    fn common_prefix<T: Copy + Ord + Eq>(lhs: &[T], rhs: &[T], reverse: bool) -> usize {
        if lhs.is_empty()
            || rhs.is_empty()
            || (!reverse && (lhs.first() != rhs.first()))
            || (reverse && (lhs.last() != rhs.last()))
        {
            return 0;
        }

        let mut pointmin = 0;
        let mut pointmax = lhs.len().min(rhs.len());
        let mut pointmid = pointmax;

        let mut pointstart = 0;

        while pointmin < pointmid {
            let (lhsrange, rhsrange) = if !reverse {
                (pointstart..pointmid, pointstart..pointmid)
            } else {
                (
                    lhs.len() - pointmid..lhs.len() - pointstart,
                    rhs.len() - pointmid..rhs.len() - pointstart,
                )
            };

            if lhs[lhsrange] == rhs[rhsrange] {
                pointmin = pointmid;
                pointstart = pointmin;
            } else {
                pointmax = pointmid;
            }

            pointmid = (pointmax - pointmin) / 2 + pointmin;
        }

        pointmid
    }

    #[inline]
    fn common_overlap(lhs: &[u8], rhs: &[u8]) -> usize {
        if lhs.is_empty() || rhs.is_empty() {
            return 0;
        }

        let minlen = lhs.len().min(rhs.len());

        // A working set with longer string truncated
        let l = if lhs.len() > rhs.len() {
            &lhs[lhs.len() - rhs.len()..]
        } else {
            lhs
        };
        let r = if lhs.len() < rhs.len() {
            &rhs[..lhs.len()]
        } else {
            rhs
        };

        // Quick check for the worst case.
        if l == r {
            return minlen;
        }

        // Start by looking for a single character match
        // and increase length until no match is found.
        // Performance analysis: https://neil.fraser.name/news/2010/11/04/
        let mut len = 1;
        let mut best = 0;

        loop {
            let pattern = &l[minlen - len..];
            let found = if let Some(found) = r
                .windows(pattern.len())
                .step_by(1)
                .position(|p| p == pattern)
            {
                found
            } else {
                return best;
            };

            len += found;
            if found == 0 || l[minlen - len..] == r[..len] {
                best = len;
                len += 1;
            }
        }
    }

    // Reduce the number of edits by eliminating semantically trivial equalities
    #[inline]
    fn cleanup_semantic(diffs: &mut Vec<Diff<u8>>) {
        let mut changes = false;

        let mut pointer = 0_usize;
        // reducing runtime allocation by giving this vec max capacity
        let mut equalities = Vec::with_capacity(diffs.len());
        let mut last_equality = None;

        // Number of bytes changed pre equality
        let mut insert_len_pre = 0_usize;
        let mut delete_len_pre = 0_usize;

        // Number of bytes changed post equality
        let mut insert_len_post = 0_usize;
        let mut delete_len_post = 0_usize;

        let mut difflen = diffs.len();

        while pointer < difflen {
            let mut diff_mod = false;
            if diffs[pointer].op() == Ops::Equal {
                equalities.push(pointer);
                // Updating pre equality changes
                insert_len_pre = insert_len_post;
                delete_len_pre = delete_len_post;

                // Resetting post insertion changes to 0
                insert_len_post = 0;
                delete_len_post = 0;

                last_equality = Some(diffs[pointer].data());
            } else {
                // Ops::Insert || Ops::Delete
                // Increasing changes of post_equality metrics
                if diffs[pointer].op() == Ops::Insert {
                    insert_len_post += diffs[pointer].size();
                } else {
                    delete_len_post += diffs[pointer].size();
                }

                // Eliminate an equality that is smaller or equal to the edits on both
                // sides of it.
                if let Some(last_eq) = &last_equality {
                    if last_eq.len() <= insert_len_pre.max(delete_len_pre)
                        && last_eq.len() <= insert_len_post.max(delete_len_post)
                    {
                        if let Some(&last) = equalities.last() {
                            // Duplicate record
                            diffs.insert(last, Diff::delete(last_eq));
                            // Change the other copy to insert
                            diffs[last + 1].0 = Ops::Insert;
                            // change diff length
                            difflen = diffs.len();

                            // Throw away the equality we just deleted.
                            equalities.pop();
                            // Throw away the previous equality (it needs to be reevaluated).
                            equalities.pop();

                            diff_mod = true;
                            changes = true;

                            if let Some(&e) = equalities.last() {
                                pointer = e;
                            } else {
                                pointer = 0;
                            }

                            // reset all counters
                            insert_len_pre = 0;
                            delete_len_pre = 0;
                            insert_len_post = 0;
                            delete_len_post = 0;

                            last_equality = None;
                        }
                    }
                }
            }

            pointer += if diff_mod && pointer == 0 { 0 } else { 1 };
        }

        // Normalize the diff
        if changes {
            Self::cleanup_merge(diffs);
        }

        Self::cleanup_semantic_lossless(diffs);

        difflen = diffs.len();

        // Find any overlaps between deletions and insertions.
        // e.g: <del>abcxxx</del><ins>xxxdef</ins>
        //   -> <del>abc</del>xxx<ins>def</ins>
        // e.g: <del>xxxabc</del><ins>defxxx</ins>
        //   -> <ins>def</ins>xxx<del>abc</del>
        // Only extract an overlap if it is as big as the edit ahead or behind it.
        pointer = 1;
        while difflen > 0 && pointer < difflen {
            if diffs[pointer - 1].op() == Ops::Delete && diffs[pointer].op() == Ops::Insert {
                let delete = diffs[pointer - 1].data().to_vec();
                let insert = diffs[pointer].data().to_vec();

                let delete_thres = delete.len() / 2 + delete.len() % 2;
                let insert_thres = insert.len() / 2 + insert.len() % 2;

                let overlap_1 = Self::common_overlap(&delete[..], &insert[..]);
                let overlap_2 = Self::common_overlap(&insert[..], &delete[..]);

                if overlap_1 >= overlap_2
                    && (overlap_1 >= delete_thres || overlap_1 >= insert_thres)
                {
                    // Overlap found.  Insert an equality and trim the surrounding edits.
                    diffs.insert(pointer, Diff::equal(&insert[..overlap_1]));
                    diffs[pointer - 1].1 = delete[..delete.len() - overlap_1].to_vec();
                    diffs[pointer + 1].1 = insert[overlap_1..].to_vec();
                    difflen = diffs.len();
                    pointer += 1;
                } else if overlap_2 >= delete_thres || overlap_2 >= insert_thres {
                    // Reverse overlap
                    // Insert equality and swap and trim surrounding edits
                    diffs.insert(pointer, Diff::equal(&delete[..overlap_2]));
                    diffs[pointer - 1] = Diff::insert(&insert[..insert.len() - overlap_2]);
                    diffs[pointer + 1] = Diff::delete(&delete[overlap_2..]);

                    difflen = diffs.len();
                    pointer += 1;
                }
                pointer += 1;
            }
            pointer += 1;
        }
    }

    // Look for single edits surrounded on both sides by equalities
    // e.g: The c<ins>at c</ins>ame. -> The <ins>cat </ins>came.
    #[inline]
    fn cleanup_semantic_lossless(diffs: &mut Vec<Diff<u8>>) {
        let mut pointer = 1_usize;
        let mut difflen = diffs.len();

        // Intentionally ignore the first and last element (don't need checking).
        while difflen > 0 && pointer < difflen - 1 {
            // an edit surrounded by equalities
            if diffs[pointer - 1].op() == Ops::Equal && diffs[pointer + 1].op() == Ops::Equal {
                // let mut equality_prev = diffs[pointer - 1].data().to_vec();
                // let mut edit = diffs[pointer].data().to_vec();
                // let mut equality_next = diffs[pointer + 1].data().to_vec();

                // Shift the edit as far left as possible
                let (mut equality_prev, mut edit, mut equality_next) = {
                    let commonlen =
                        Self::common_prefix(diffs[pointer - 1].data(), diffs[pointer].data(), true);
                    if commonlen > 0 {
                        let common = &diffs[pointer].data()[diffs[pointer].size() - commonlen..];

                        (
                            diffs[pointer - 1].data()[..diffs[pointer - 1].size() - commonlen]
                                .to_vec(),
                            [
                                common,
                                &diffs[pointer].data()[..diffs[pointer].size() - commonlen],
                            ]
                            .concat(),
                            [common, diffs[pointer + 1].data()].concat(),
                        )
                    } else {
                        (
                            diffs[pointer - 1].data().to_vec(),
                            diffs[pointer].data().to_vec(),
                            diffs[pointer + 1].data().to_vec(),
                        )
                    }
                };

                // Step byte by byte right looking for the best fit
                let mut best_equality_prev = equality_prev.clone();
                let mut best_edit = edit.clone();
                let mut best_equality_next = equality_next.clone();

                let mut best_score = Self::cleanup_semantic_score(&equality_prev[..], &edit[..])
                    + Self::cleanup_semantic_score(&edit[..], &equality_next[..]);

                while !edit.is_empty() && !equality_next.is_empty() && edit[0] == equality_next[0] {
                    equality_prev.push(edit[0]);
                    edit.remove(0);
                    edit.push(equality_next[0]);

                    equality_next.remove(0);

                    let score = Self::cleanup_semantic_score(&equality_prev[..], &edit[..])
                        + Self::cleanup_semantic_score(&edit[..], &equality_next[..]);

                    // The >= encourages trailing rather than leading whitespace on edits.
                    if score >= best_score {
                        best_score = score;
                        best_equality_prev.clone_from(&equality_prev);
                        best_edit.clone_from(&edit);
                        best_equality_next.clone_from(&equality_next);
                    }
                }

                // We have an improvement, save it back to the diff.
                if diffs[pointer - 1].data() != best_equality_prev {
                    if !best_equality_prev.is_empty() {
                        diffs[pointer - 1].1.clone_from(&best_equality_prev);
                    } else {
                        diffs.remove(pointer - 1);
                        pointer -= 1;
                        difflen = diffs.len();
                    }

                    diffs[pointer].1.clone_from(&best_edit);

                    if !best_equality_next.is_empty() {
                        diffs[pointer + 1].1.clone_from(&best_equality_next);
                    } else {
                        diffs.remove(pointer + 1);
                        pointer -= 1;
                        difflen = diffs.len();
                    }
                }
            }

            pointer += 1;
        }
    }

    // Given two strings, compute a score representing whether the internal
    // boundary falls on logical boundaries
    // Scores range from 6 (best) to 0 (worst)
    #[inline]
    fn cleanup_semantic_score(one: &[u8], two: &[u8]) -> u8 {
        let (char1, char2) = if let (Some(&char1), Some(&char2)) = (one.last(), two.first()) {
            (char1 as char, char2 as char)
        } else {
            return 6;
        };

        // Each port of this function behaves slightly differently due to
        // subtle differences in each language's definition of things like
        // 'whitespace'.  Since this function's purpose is largely cosmetic,
        // the choice has been made to use each language's native features
        // rather than force total conformity.

        let whitespace_1 = char1.is_whitespace();
        let whitespace_2 = char2.is_whitespace();

        let linebreak_1 = whitespace_1 && (char1 == '\n' || char1 == '\r');
        let linebreak_2 = whitespace_2 && (char2 == '\n' || char2 == '\r');

        let blankline_1 = linebreak_1 && (one.ends_with(b"\n\n") || (one.ends_with(b"\n\r\n")));
        let blankline_2 = linebreak_2
            && (two.starts_with(b"\r\n\n")
                || two.starts_with(b"\r\n\r\n")
                || two.starts_with(b"\n\r\n")
                || two.starts_with(b"\n\n"));

        if blankline_1 || blankline_2 {
            // 5 for blank lines
            5
        } else if linebreak_1 || linebreak_2 {
            // Four points for line breaks.
            4
        } else if !char1.is_alphanumeric() && !whitespace_1 && whitespace_2 {
            // Three points for end of sentences.
            3
        } else if whitespace_1 || whitespace_2 {
            // 2 for whitespace
            2
        } else if !char1.is_alphanumeric() || !char2.is_alphanumeric() {
            // 1 for not alphanumeric
            1
        } else {
            0
        }
    }

    // Reorder and merge like edit sections.  Merge equalities.
    // Any edit section can move as long as it doesn't cross an equality.
    #[inline]
    fn cleanup_merge<T: BisectSplit>(diffs: &mut Vec<Diff<T>>) {
        // Push a dummy diff ... this triggers the equality as a last step
        diffs.push(Diff::equal(&[]));

        let mut difflen = diffs.len();

        let mut pointer = 0_usize;

        let mut insert_n = 0;
        let mut delete_n = 0;

        let mut insert_data = vec![];
        let mut delete_data = vec![];

        while pointer < difflen {
            match diffs[pointer].op() {
                Ops::Insert => {
                    insert_n += 1;
                    insert_data = [&insert_data[..], diffs[pointer].data()].concat();
                    pointer += 1;
                }
                Ops::Delete => {
                    delete_n += 1;
                    delete_data = [&delete_data[..], diffs[pointer].data()].concat();
                    pointer += 1;
                }
                Ops::Equal => {
                    // Upon reaching an equality, check for prior redundancies.
                    if delete_n + insert_n > 1 {
                        if delete_n != 0 && insert_n != 0 {
                            // Factor out any common prefixies.
                            let commonlen =
                                Self::common_prefix(&insert_data[..], &delete_data[..], false);
                            if commonlen != 0 {
                                let tmpidx = pointer - delete_n - insert_n;
                                if tmpidx > 0 && diffs[tmpidx - 1].op() == Ops::Equal {
                                    diffs[tmpidx - 1].1 =
                                        [&diffs[tmpidx - 1].1[..], &insert_data[..commonlen]]
                                            .concat();
                                } else {
                                    diffs.insert(0, Diff::equal(&insert_data[..commonlen]));
                                    pointer += 1;
                                }
                                insert_data = insert_data[commonlen..].to_vec();
                                delete_data = delete_data[commonlen..].to_vec();
                            }

                            // Factor out any common suffixies.
                            let commonlen =
                                Self::common_prefix(&insert_data[..], &delete_data[..], true);
                            if commonlen > 0 {
                                diffs[pointer].1 = [
                                    &insert_data[insert_data.len() - commonlen..],
                                    diffs[pointer].data(),
                                ]
                                .concat();
                                insert_data = insert_data[..insert_data.len() - commonlen].to_vec();
                                delete_data = delete_data[..delete_data.len() - commonlen].to_vec();
                            }
                        }

                        // Delete the offending records and add the merged ones.
                        pointer -= delete_n + insert_n;

                        // Reversing because index will not change
                        (pointer..pointer + delete_n + insert_n)
                            .rev()
                            .for_each(|i| {
                                diffs.remove(i);
                            });
                        difflen = diffs.len();

                        if !delete_data.is_empty() {
                            diffs.insert(pointer, Diff::delete(&delete_data));
                            pointer += 1;
                            difflen = diffs.len();
                        }

                        if !insert_data.is_empty() {
                            diffs.insert(pointer, Diff::insert(&insert_data));
                            pointer += 1;
                            difflen = diffs.len();
                        }

                        pointer += 1;
                    } else if pointer != 0 && diffs[pointer - 1].op() == Ops::Equal {
                        // Merge this equality with the previous one.;
                        diffs[pointer - 1].1 =
                            [&diffs[pointer - 1].1[..], diffs[pointer].data()].concat();
                        diffs.remove(pointer);

                        difflen = diffs.len();
                    } else {
                        pointer += 1;
                    }

                    insert_n = 0;
                    delete_n = 0;
                    insert_data = Vec::new();
                    delete_data = Vec::new();
                }
            }
        }

        if let Some(dl) = diffs.last() {
            if dl.data().is_empty() {
                diffs.pop();
            }
        }

        difflen = diffs.len();

        // Second pass: look for single edits surrounded on both sides by equalities
        // which can be shifted sideways to eliminate an equality.
        // e.g: A<ins>BA</ins>C -> <ins>AB</ins>AC
        pointer = 1;
        let mut changes = false;

        while difflen > 0 && pointer < difflen - 1 {
            if let (Some(diff_prev), Some(diff), Some(diff_next)) = (
                diffs.get(pointer - 1),
                diffs.get(pointer),
                diffs.get(pointer + 1),
            ) {
                // This is a single edit surrounded by equalities.
                if diff_prev.op() == Ops::Equal && diff_next.op() == Ops::Equal {
                    let substr_idx = if diff.size() >= diff_prev.size() {
                        diff.size() - diff_prev.size()
                    } else {
                        0
                    };
                    if &diff.data()[substr_idx..] == diff_prev.data() {
                        // Shift the edit over the previous equality.
                        let new_current_data = [
                            diff_prev.data(),
                            &diff.data()[..diff.size() - diff_prev.size()],
                        ]
                        .concat();
                        let new_next_data = [diff_prev.data(), diff_next.data()].concat();

                        diffs[pointer].1 = new_current_data;
                        diffs[pointer + 1].1 = new_next_data;
                        diffs.remove(pointer - 1);
                        difflen = diffs.len();

                        changes = true;
                    } else if &diff.data()[..if diff_next.size() <= diff.size() {
                        diff_next.size()
                    } else {
                        diff.size()
                    }] == diff_next.data()
                    {
                        // Shift the edit over the next equality.
                        diffs[pointer - 1].1 =
                            [&diffs[pointer - 1].1[..], diffs[pointer + 1].data()].concat();
                        diffs[pointer].1 = [
                            &diffs[pointer].data()[diffs[pointer + 1].size()..],
                            diffs[pointer + 1].data(),
                        ]
                        .concat();
                        diffs.remove(pointer + 1);
                        difflen = diffs.len();

                        changes = true;
                    }
                }
            }

            pointer += 1;
        }

        if changes {
            Self::cleanup_merge(diffs);
        }
    }

    // Reduce the number of edits by eliminating operationally trivial equalities.
    fn cleanup_efficiency(&self, diffs: &mut Vec<Diff<u8>>) {
        if diffs.is_empty() {
            return;
        }

        let mut changes = false;
        // let mut equalities = vec![];
        // let mut last_equality = None;

        let mut idx = 0;
        while idx < diffs.len() {
            let diff = &diffs[idx];
            // if diff.op() == Ops::Equal {
            //     // Equality found
            //     if diff.size()
            // }
            idx += 1;
        }
        // if (diffs.isEmpty()) {
        //     return;
        //   }
        //   bool changes = false;
        //   QStack<Diff<u8>> equalities;  // Stack of equalities.
        //   QString lastequality;  // Always equal to equalities.lastElement().text
        //   QMutableListIterator<Diff<u8>> pointer(diffs);
        //   // Is there an insertion operation before the last equality.
        //   bool pre_ins = false;
        //   // Is there a deletion operation before the last equality.
        //   bool pre_del = false;
        //   // Is there an insertion operation after the last equality.
        //   bool post_ins = false;
        //   // Is there a deletion operation after the last equality.
        //   bool post_del = false;

        //   Diff *thisDiff = pointer.hasNext() ? &pointer.next() : NULL;
        //   Diff *safeDiff = thisDiff;

        //   while (thisDiff != NULL) {
        //     if (thisDiff->operation == EQUAL) {
        //       // Equality found.
        //       if (thisDiff->text.length() < Diff_EditCost && (post_ins || post_del)) {
        //         // Candidate found.
        //         equalities.push(*thisDiff);
        //         pre_ins = post_ins;
        //         pre_del = post_del;
        //         lastequality = thisDiff->text;
        //       } else {
        //         // Not a candidate, and can never become one.
        //         equalities.clear();
        //         lastequality = QString();
        //         safeDiff = thisDiff;
        //       }
        //       post_ins = post_del = false;
        //     } else {
        //       // An insertion or deletion.
        //       if (thisDiff->operation == DELETE) {
        //         post_del = true;
        //       } else {
        //         post_ins = true;
        //       }
        //       /*
        //       * Five types to be split:
        //       * <ins>A</ins><del>B</del>XY<ins>C</ins><del>D</del>
        //       * <ins>A</ins>X<ins>C</ins><del>D</del>
        //       * <ins>A</ins><del>B</del>X<ins>C</ins>
        //       * <ins>A</del>X<ins>C</ins><del>D</del>
        //       * <ins>A</ins><del>B</del>X<del>C</del>
        //       */
        //       if (!lastequality.isNull()
        //           && ((pre_ins && pre_del && post_ins && post_del)
        //           || ((lastequality.length() < Diff_EditCost / 2)
        //           && ((pre_ins ? 1 : 0) + (pre_del ? 1 : 0)
        //           + (post_ins ? 1 : 0) + (post_del ? 1 : 0)) == 3))) {
        //         // printf("Splitting: '%s'\n", qPrintable(lastequality));
        //         // Walk back to offending equality.
        //         while (*thisDiff != equalities.top()) {
        //           thisDiff = &pointer.previous();
        //         }
        //         pointer.next();

        //         // Replace equality with a delete.
        //         pointer.setValue(Diff(DELETE, lastequality));
        //         // Insert a corresponding an insert.
        //         pointer.insert(Diff(INSERT, lastequality));
        //         thisDiff = &pointer.previous();
        //         pointer.next();

        //         equalities.pop();  // Throw away the equality we just deleted.
        //         lastequality = QString();
        //         if (pre_ins && pre_del) {
        //           // No changes made which could affect previous entry, keep going.
        //           post_ins = post_del = true;
        //           equalities.clear();
        //           safeDiff = thisDiff;
        //         } else {
        //           if (!equalities.isEmpty()) {
        //             // Throw away the previous equality (it needs to be reevaluated).
        //             equalities.pop();
        //           }
        //           if (equalities.isEmpty()) {
        //             // There are no previous questionable equalities,
        //             // walk back to the last known safe diff.
        //             thisDiff = safeDiff;
        //           } else {
        //             // There is an equality we can fall back to.
        //             thisDiff = &equalities.top();
        //           }
        //           while (*thisDiff != pointer.previous()) {
        //             // Intentionally empty loop.
        //           }
        //           post_ins = post_del = false;
        //         }

        //         changes = true;
        //       }
        //     }
        //     thisDiff = pointer.hasNext() ? &pointer.next() : NULL;
        //   }

        //   if (changes) {
        //     diff_cleanupMerge(diffs);
        //   }
        todo!()
    }

    #[inline]
    fn x_index<T: Copy + Eq + Ord>(diffs: &[Diff<T>], loc: usize) -> usize {
        let mut char1 = 0;
        let mut char2 = 0;

        let mut last_char1 = 0;
        let mut last_char2 = 0;

        let mut last_diff = None;

        for diff in diffs.iter() {
            if diff.op() != Ops::Insert {
                // Equality or deletion
                char1 += diff.size();
            }

            if diff.op() != Ops::Delete {
                // Equality or insertion
                char2 += diff.size();
            }

            if char1 > loc {
                // overshot location
                last_diff = Some(diff);
                break;
            }

            last_char1 = char1;
            last_char2 = char2;
        }

        if let Some(ld) = last_diff {
            if ld.op() == Ops::Delete {
                // The location was deleted.
                return last_char2;
            }
        }

        // Add the remaining character length.
        last_char2 + (loc - last_char1)
    }

    fn diff_text_old(diffs: &[Diff<u8>]) -> Vec<u8> {
        diffs
            .iter()
            .filter_map(|diff| {
                if diff.op() != Ops::Insert {
                    Some(diff.data())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
            .concat()
    }

    fn diff_text_new(diffs: &[Diff<u8>]) -> Vec<u8> {
        diffs
            .iter()
            .filter_map(|diff| {
                if diff.op() != Ops::Delete {
                    Some(diff.data())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
            .concat()
    }

    // Look through the patches and break up any which are longer than the maximum
    // limit of the match algorithm.
    // Intended to be called only from within patch_apply.
    #[inline]
    fn split_max(&self, patches: &mut Patches) {
        let max_bit = self.match_max_bits();
        let patch_margin = self.patch_margin() as usize;

        let mut idx = 0;
        while idx < patches.len() {
            if patches[idx].length1 <= max_bit {
                idx += 1;
                continue;
            }

            let mut bigpatch = patches.remove(idx);
            let mut start1 = bigpatch.start1;
            let mut start2 = bigpatch.start2;

            let mut precontext = vec![];
            let mut patch_to_ins = vec![];

            while !bigpatch.diffs.is_empty() {
                let mut patch = Patch::default();
                let mut empty = true;

                patch.start1 = start1 - precontext.len();
                patch.start2 = start2 - precontext.len();
                if !precontext.is_empty() {
                    patch.length1 = precontext.len();
                    patch.length2 = precontext.len();

                    patch.diffs.push(Diff::equal(&precontext));
                }

                while !bigpatch.diffs.is_empty() && patch.length1 < max_bit - patch_margin {
                    if bigpatch.diffs[0].op() == Ops::Insert {
                        // Insertions are harmless.
                        patch.length2 += bigpatch.diffs[0].size();
                        start2 += bigpatch.diffs[0].size();
                        let d = bigpatch.diffs.remove(0);
                        patch.diffs.push(d);
                        empty = false;
                        // patch.diffs.push(value)
                    } else if bigpatch.diffs[0].op() == Ops::Delete
                        && patch.diffs.len() == 1
                        && patch.diffs[0].op() == Ops::Equal
                        && bigpatch.diffs[0].size() > 2 * max_bit
                    {
                        // This is a large deletion.  Let it pass in one chunk.
                        patch.length1 += bigpatch.diffs[0].size();
                        start1 += bigpatch.diffs[0].size();
                        empty = false;
                        patch
                            .diffs
                            .push(Diff::new(bigpatch.diffs[0].op(), bigpatch.diffs[0].data()));
                        bigpatch.diffs.remove(0);
                    } else {
                        // Deletion or equality.  Only take as much as we can stomach.
                        let diff_text = bigpatch.diffs[0].data()[..bigpatch.diffs[0]
                            .size()
                            .min(max_bit - patch.length1 - patch_margin)]
                            .to_vec();
                        patch.length1 += diff_text.len();
                        start1 += diff_text.len();

                        if bigpatch.diffs[0].op() == Ops::Equal {
                            patch.length2 += diff_text.len();
                            start2 += diff_text.len();
                        } else {
                            empty = false;
                        }

                        patch
                            .diffs
                            .push(Diff::new(bigpatch.diffs[0].op(), &diff_text));

                        let cond = if let Some(d) = bigpatch.diffs.first() {
                            diff_text == d.data()
                        } else {
                            false
                        };

                        if cond {
                            bigpatch.diffs.remove(0);
                        } else if let Some(bd) = bigpatch.diffs.first_mut() {
                            bd.1 = bd.data()[diff_text.len()..].to_vec();
                        }
                    }
                }

                // Compute the head context for the next patch.
                precontext = Self::diff_text_new(&patch.diffs);
                if precontext.len() > patch_margin {
                    precontext = precontext[precontext.len() - patch_margin..].to_vec();
                }

                // Append the end context for this patch.
                let mut postcontext = Self::diff_text_old(&bigpatch.diffs);
                // [0 .. patch_margin.min(other)].to_vec()
                if patch_margin < postcontext.len() {
                    postcontext = postcontext[..patch_margin].to_vec();
                }

                if !postcontext.is_empty() {
                    patch.length1 += postcontext.len();
                    patch.length2 += postcontext.len();

                    let other = if let Some(pd) = patch.diffs.last_mut() {
                        if pd.op() == Ops::Equal {
                            pd.1 = [&pd.1, &postcontext[..]].concat();
                            false
                        } else {
                            true
                        }
                    } else {
                        true
                    };

                    if other {
                        patch.diffs.push(Diff::equal(&postcontext));
                    }
                }

                if !empty {
                    patch_to_ins.push(patch);
                }
            }

            if !patch_to_ins.is_empty() {
                patches.splice(idx..idx, patch_to_ins.into_iter());
            }

            idx += 1;
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
struct LineToChars<'a> {
    chars_old: Vec<usize>,
    chars_new: Vec<usize>,
    lines: Vec<&'a [u8]>,
}

impl DiffMatchPatch {
    #[inline]
    fn lines_to_chars<'a>(old: &'a [u8], new: &'a [u8]) -> LineToChars<'a> {
        let mut lines: Vec<&'a [u8]> = vec![];
        let mut linehash: HashMap<&'a [u8], usize> = HashMap::new();

        // Allocate 2/3rds of the UTF16::MAX (65535) value space for text1, the rest for text2.
        // let mut maxlines = 5;
        let mut maxlines = 40000;
        let chars_old = Self::lines_to_chars_internal(old, &mut lines, &mut linehash, maxlines);

        // This basically represents the U16::MAX value
        maxlines = 65535;
        // maxlines = 7;
        let chars_new = Self::lines_to_chars_internal(new, &mut lines, &mut linehash, maxlines);

        LineToChars {
            chars_old,
            chars_new,
            lines,
        }
    }

    #[inline]
    fn lines_to_chars_internal<'a>(
        text: &'a [u8],
        array: &mut Vec<&'a [u8]>,
        hash: &mut HashMap<&'a [u8], usize>,
        maxlines: usize,
    ) -> Vec<usize> {
        let take = maxlines - array.len();

        // let mut lines = ;
        let mut charlist = Vec::with_capacity(take);

        let mut broke = false;
        let mut cursor = 0;

        text.split_inclusive(|u| *u == b'\n')
            .enumerate()
            .take(take)
            .for_each(|(idx, line)| {
                cursor += line.len();

                let e = hash.entry(line).or_insert(array.len());
                // Fresh insert
                if *e == array.len() {
                    array.push(line)
                }

                // upcasting, should never fail
                charlist.push(*e);

                // break at max lines
                broke = idx == take - 1;
            });

        // We broke at max lines, so we'll account for the remaining text
        if broke {
            let line = &text[cursor..];
            let e = hash.entry(line).or_insert(array.len());
            // Fresh insert
            if *e == array.len() {
                array.push(line)
            }

            // upcasting should never fail
            charlist.push(*e);
        }
        charlist
    }

    #[inline]
    fn chars_to_lines(diffs: &[Diff<usize>], lines: &[&[u8]]) -> Vec<Diff<u8>> {
        diffs
            .iter()
            .map(|d| {
                let t = d
                    .data()
                    .iter()
                    .map(|&d| {
                        *lines.get(d).unwrap() // investigate
                    })
                    .collect::<Vec<_>>()
                    .concat();

                Diff::new(d.op(), &t)
            })
            .collect::<Vec<_>>()

        // res
    }
}

// Match methods
impl DiffMatchPatch {
    fn match_internal(&self, text: &[u8], pattern: &[u8], loc: usize) -> Option<usize> {
        // Check for null inputs.
        // Nothing to match.
        if text.is_empty() {
            return None;
        }

        // loc = Math.max(0, Math.min(loc, text.length));
        let loc = loc.min(text.len());

        if text == pattern {
            // Shortcut (potentially not guaranteed by the algorithm)
            Some(0)
        } else if &text[loc..(loc + pattern.len()).min(text.len())] == pattern {
            // Perfect match at the perfect spot!  (Includes case of null pattern)
            Some(loc)
        } else {
            // Do a fuzzy compare.
            // return this.match_bitap_(text, pattern, loc);
            self.match_bitap(text, pattern, loc)
        }
    }

    fn match_bitap(&self, text: &[u8], pattern: &[u8], loc: usize) -> Option<usize> {
        if pattern.len() > self.match_max_bits() {
            todo!("Throw error");
        }

        let alphabet = Self::match_alphabet(pattern);

        // Highest score beyond which we give up.
        let mut score_thres = self.match_threshold();

        // Is there a nearby exact match? (speedup)
        if let Some(best_loc) = text
            .windows(pattern.len())
            .step_by(1)
            .skip(loc)
            .position(|p| p == pattern)
            .map(|pos| pos + loc)
        {
            score_thres = self
                .bitap_score(loc, pattern.len(), 0, best_loc)
                .min(score_thres);

            // What about in the other direction? (speedup)
            // best_loc = text.lastIndexOf(pattern, loc + pattern.length);
            if let Some(best_loc_rev) = text
                .windows(pattern.len())
                .step_by(1)
                .skip(loc)
                .rev()
                .position(|p| p == pattern)
                .map(|pos| text.len() - pos - pattern.len())
            {
                score_thres = self.bitap_score(loc, pattern.len(), 0, best_loc_rev);
            }
        }

        // Initialise the bit arrays.
        let matchmask = 1 << (pattern.len() - 1);

        // var matchmask = 1 << (pattern.length - 1);
        let mut best_loc = None;

        let mut bin_min;
        let mut bin_mid;
        let mut bin_max = pattern.len() + text.len();
        let mut last_rd = vec![];

        for d in 0..pattern.len() {
            // Scan for the best match; each iteration allows for one more error.
            // Run a binary search to determine how far from 'loc' we can stray at
            // this error level.
            bin_min = 0;
            bin_mid = bin_max;

            while bin_min < bin_mid {
                let score = self.bitap_score(loc, pattern.len(), d, loc + bin_mid);
                if score <= score_thres {
                    bin_min = bin_mid;
                } else {
                    bin_max = bin_mid;
                }

                bin_mid = (bin_max - bin_min) / 2 + bin_min;
            }

            // Use the result from this iteration as the maximum for the next.
            bin_max = bin_mid;
            let mut start = if loc > bin_mid {
                1.max(loc - bin_mid + 1)
            } else {
                1
            };
            let finish = (loc + bin_mid).min(text.len()) + pattern.len();

            let mut rd = vec![0; finish + 2];
            rd[finish + 1] = (1 << d) - 1;

            let mut j = finish;
            while j >= start {
                let char_match = if text.len() < j {
                    0
                } else {
                    alphabet.get(&text[j - 1]).map_or(0, |&v| v)
                };

                rd[j] = if d == 0 {
                    // first pass: exact match
                    ((rd[j + 1] << 1) | 1) & char_match
                } else {
                    // Subsequent passes: fuzzy match.
                    ((rd[j + 1] << 1) | 1) & char_match
                        | (((last_rd[j + 1] | last_rd[j]) << 1) | 1_usize)
                        | last_rd[j + 1]
                };

                if (rd[j] & matchmask) != 0 {
                    let score = self.bitap_score(loc, pattern.len(), d, j - 1);
                    // This match will almost certainly be better than any existing
                    // match.  But check anyway.
                    if score <= score_thres {
                        score_thres = score;
                        let bst_loc = j - 1;

                        best_loc = Some(bst_loc);
                        if bst_loc > loc {
                            // When passing loc, don't exceed our current distance from loc.
                            start = 1.max(if loc > bst_loc { loc - bst_loc } else { 0 });
                        } else {
                            // Already passed loc, downhill from here on in.
                            break;
                        }
                    }
                }

                j -= 1;
            }
            // No hope for a (better) match at greater error levels.
            if self.bitap_score(loc, pattern.len(), d + 1, loc) > score_thres {
                break;
            }
            last_rd.clone_from(&rd);
        }

        best_loc
    }

    fn match_alphabet(pattern: &[u8]) -> HashMap<u8, usize> {
        let mut map = HashMap::with_capacity(pattern.len());

        pattern.iter().enumerate().for_each(|(i, &p)| {
            let v = map.entry(p).or_insert(0_usize);
            *v |= 1 << (pattern.len() - i - 1)
        });

        map
    }

    // Compute and return the score for a match with e errors and x location.
    // Accesses loc and pattern through being a closure.
    fn bitap_score(&self, org_loc: usize, pattern_len: usize, errs: usize, loc: usize) -> f32 {
        let accuracy = errs as f32 / pattern_len as f32;
        let proximity = (org_loc as i32 - loc as i32).abs();

        if self.match_distance() == 0 {
            if proximity > 0 {
                return 1.;
            } else {
                return accuracy;
            }
        }

        accuracy + proximity as f32 / self.match_distance() as f32
    }
}

// Patch Methods
#[derive(Debug, Default, Clone)]
pub struct Patch {
    diffs: Vec<Diff<u8>>,
    start1: usize,
    start2: usize,
    length1: usize,
    length2: usize,
}

impl Display for Patch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let coord1 = if self.length1 == 0 {
            format!("{},0", self.start1)
        } else {
            format!(
                "{}{}",
                self.start1 + 1,
                if self.length1 != 1 {
                    format!(",{}", self.length1)
                } else {
                    String::new()
                }
            )
        };

        let coord2 = if self.length2 == 0 {
            format!("{},0", self.start2)
        } else {
            format!(
                "{}{}",
                self.start2 + 1,
                if self.length2 != 1 {
                    format!(",{}", self.length2)
                } else {
                    String::new()
                }
            )
        };

        let mut segments = vec![
            "@@ -".to_string(),
            coord1,
            " +".to_string(),
            coord2,
            " @@\n".to_string(),
        ];
        self.diffs.iter().for_each(|diff| {
            let sign = match diff.op() {
                Ops::Insert => '+',
                Ops::Delete => '-',
                Ops::Equal => ' ',
            };

            let segment = format!("{sign}{}\n", percent_encode(diff.data(), CONTROLS));

            segments.push(segment)
        });

        write!(f, "{}", segments.join(""))
    }
}

pub enum PatchInput<'a> {
    Texts(&'a str, &'a str),
    Diffs(&'a [Diff<u8>]),
    TextDiffs(&'a str, &'a [Diff<u8>]),
}

pub type Patches = Vec<Patch>;

impl DiffMatchPatch {
    fn parse_patch_header(s: &[u8]) -> Option<(usize, Option<usize>, usize, Option<usize>)> {
        let mut section = Vec::with_capacity(64);
        let mut current_sect = 0;

        let mut old_line = 0;
        let mut old_cols = None;
        let mut new_line = 0;
        let mut new_cols = None;

        for &c in s.iter() {
            if c == b' ' {
                match current_sect {
                    0 => {
                        if &section != b"@@" {
                            return None;
                        }
                    }
                    1 => {
                        if section.is_empty() {
                            return None;
                        }

                        let splits = section[1..].split(|&p| p == b',').collect::<Vec<_>>();

                        let ol = splits.first()?;
                        old_line = std::str::from_utf8(ol).ok()?.parse::<usize>().ok()?;
                        if let Some(&oc) = splits.get(1) {
                            old_cols = Some(std::str::from_utf8(oc).ok()?.parse::<usize>().ok()?);
                        }
                    }
                    2 => {
                        let splits = section[if *section.first()? == b'+' { 1 } else { 0 }..]
                            .split(|&p| p == b',')
                            .collect::<Vec<_>>();

                        let nl = splits.first()?;
                        new_line = std::str::from_utf8(nl).ok()?.parse::<usize>().ok()?;
                        if let Some(&nc) = splits.get(1) {
                            new_cols = Some(std::str::from_utf8(nc).ok()?.parse::<usize>().ok()?);
                        }
                    }
                    _ => {
                        // invalid pattern
                        return None;
                    }
                }

                section = Vec::with_capacity(64);
                current_sect += 1;
                continue;
            }

            if current_sect == 1 && section.is_empty() && c != b'-' {
                return None;
            }

            section.push(c);
        }

        if &section != b"@@" {
            return None;
        }

        Some((old_line, old_cols, new_line, new_cols))
    }

    fn patch_make_internal(
        &self,
        txt: &[u8],
        diffs: &[Diff<u8>],
    ) -> Result<Patches, crate::errors::Error> {
        // No diffs -> no patches
        if diffs.is_empty() {
            return Ok(Vec::new());
        }

        let patch_margin = self.patch_margin() as usize;

        let mut patches = vec![];

        let mut patch = Patch::default();
        let mut char_n1 = 0;
        let mut char_n2 = 0;

        let mut prepatch: Vec<u8> = txt.to_vec();
        let mut postpatch: Vec<u8> = prepatch.clone();

        diffs.iter().enumerate().for_each(|(idx, diff)| {
            // a new patch starts here
            if patch.diffs.is_empty() && diff.op() != Ops::Equal {
                patch.start1 = char_n1;
                patch.start2 = char_n2;
            }

            match diff.op() {
                Ops::Insert => {
                    patch.length2 += diff.size();
                    postpatch =
                        [&postpatch[..char_n2], diff.data(), &postpatch[char_n2..]].concat();
                    patch.diffs.push(diff.clone());
                }
                Ops::Delete => {
                    patch.length1 += diff.size();
                    postpatch =
                        [&postpatch[..char_n2], &postpatch[char_n2 + diff.size()..]].concat();

                    patch.diffs.push(diff.clone());
                }
                Ops::Equal => {
                    if diff.size() <= 2 * patch_margin
                        && !patch.diffs.is_empty()
                        && diffs.len() != idx + 1
                    {
                        // Small equality inside a patch.
                        patch.length1 += diff.size();
                        patch.length2 += diff.size();

                        patch.diffs.push(diff.clone());
                    } else if diff.size() >= 2 * patch_margin && !patch.diffs.is_empty() {
                        // Time for a new patch.
                        self.patch_add_context(&mut patch, &prepatch);
                        patches.push(patch.clone());
                        patch = Patch::default();

                        // Unlike Unidiff, our patch lists have a rolling context.
                        // http://code.google.com/p/google-diff-match-patch/wiki/Unidiff
                        // Update prepatch text & pos to reflect the application of the
                        // just completed patch.
                        prepatch.clone_from(&postpatch);
                        char_n1 = char_n2;
                    }
                }
            }

            if diff.op() != Ops::Insert {
                char_n1 += diff.size();
            }
            if diff.op() != Ops::Delete {
                char_n2 += diff.size();
            }
        });

        // Pick up the leftover patch if not empty.
        if !patch.diffs.is_empty() {
            self.patch_add_context(&mut patch, &prepatch);
            patches.push(patch);
        }

        Ok(patches)
    }

    fn patch_add_context(&self, patch: &mut Patch, text: &[u8]) {
        if text.is_empty() {
            return;
        }

        let patch_margin = self.patch_margin() as usize;

        let mut pattern = &text[patch.start2..patch.start2 + patch.length1];
        let mut padding = 0;

        // Look for the first and last matches of pattern in text.  If two different
        // matches are found, increase the pattern length.
        while pattern.is_empty()
            || (text.windows(pattern.len()).position(|p| p == pattern)
                != text
                    .windows(pattern.len())
                    .rev()
                    .position(|p| p == pattern)
                    .map(|p| text.len() - p - pattern.len())
                && pattern.len() < self.match_max_bits() - patch_margin * 2)
        {
            padding += patch_margin;

            let begin = if patch.start2 > padding {
                patch.start2 - padding
            } else {
                0
            };
            let end = patch.start2 + patch.length1 + padding;

            pattern = &text[begin..if end > text.len() { text.len() } else { end }];
        }

        // Add one chunk for good luck.
        padding += patch_margin;

        // Add prefix
        let begin = if patch.start2 > padding {
            patch.start2 - padding
        } else {
            0
        };
        let prefix = &text[begin..patch.start2];
        if !prefix.is_empty() {
            patch.diffs.insert(0, Diff::equal(prefix));
        }

        // Add the suffix
        let begin = patch.start2 + patch.length1;
        let end = patch.start2 + patch.length1 + padding;
        let suffix = &text[if begin < text.len() {
            begin
        } else {
            text.len()
        }..if end < text.len() { end } else { text.len() }];
        if !suffix.is_empty() {
            patch.diffs.push(Diff::equal(suffix));
        }
        // Roll back the start points.
        patch.start1 -= prefix.len();
        patch.start2 -= prefix.len();

        // extend the lengths
        patch.length1 += prefix.len() + suffix.len();
        patch.length2 += prefix.len() + suffix.len();
    }

    fn patch_apply_internal(
        &self,
        patches: &Patches,
        source: &[u8],
    ) -> Result<(Vec<u8>, Vec<bool>), crate::errors::Error> {
        if patches.is_empty() {
            return Ok((source.to_vec(), vec![]));
        }

        let deadline = self.deadline();

        // To avoid changes to original patches!!
        let mut patches = patches.clone();

        let null_pad = self.patch_add_padding(&mut patches);
        let mut source = [&null_pad, source, &null_pad].concat();

        self.split_max(&mut patches);

        // delta keeps track of the offset between the expected and actual location
        // of the previous patch.  If there are patches expected at positions 10 and
        // 20, but the first patch was found at 12, delta is 2 and the second patch
        // has an effective expected position of 22.
        let mut delta = 0;
        let mut results = vec![false; patches.len()];

        // patches.iter().enumerate().for_each(|(x, p)| {
        for (x, p) in patches.iter().enumerate() {
            let expected_loc = p.start2 + delta;
            let txt_old = Self::diff_text_old(&p.diffs);
            let (start_loc, end_loc) = if txt_old.len() > self.match_max_bits() {
                // patch_splitMax will only provide an oversized pattern in the case of
                // a monster delete.
                if let Some(sl) =
                    self.match_internal(&source, &txt_old[..self.match_max_bits()], expected_loc)
                {
                    let el = self.match_internal(
                        &source,
                        &txt_old[txt_old.len() - self.match_max_bits()..],
                        expected_loc + txt_old.len() - self.match_max_bits(),
                    );

                    if el.is_none() || Some(sl) >= el {
                        // Can't find valid trailing context.  Drop this patch.
                        (None, el)
                    } else {
                        (Some(sl), el)
                    }
                } else {
                    (None, None)
                }
            } else {
                (self.match_internal(&source, &txt_old, expected_loc), None)
            };

            if let Some(sl) = start_loc {
                // Found a match.  :)
                results[x] = true;
                delta = sl - expected_loc;

                let txt_new = if let Some(el) = end_loc {
                    // safeMid(text, start_loc, end_loc + Match_MaxBits - start_loc);
                    &source[sl..el + self.match_max_bits()]
                } else {
                    &source[sl..sl + txt_old.len()]
                };

                if txt_old == txt_new {
                    // Perfect match, just shove the replacement text in.
                    source = [
                        &source[..sl],
                        &Self::diff_text_new(&p.diffs),
                        &source[sl + txt_old.len()..],
                    ]
                    .concat();
                } else {
                    // Imperfect match.  Run a diff to get a framework of equivalent indices.
                    let mut diffs = self.diff_internal(&txt_old, txt_new, false, deadline)?;
                    if txt_old.len() > self.match_max_bits()
                        && (Self::diff_levenshtein(&diffs) as f32 / txt_old.len() as f32)
                            > self.delete_threshold()
                    {
                        // The end points match, but the content is unacceptably bad.
                        results[x] = false;
                    } else {
                        Self::cleanup_semantic_lossless(&mut diffs);
                        let mut idx1 = 0;
                        p.diffs.iter().for_each(|diff| {
                            if diff.op() != Ops::Equal {
                                let idx2 = Self::x_index(&diffs, idx1);
                                if diff.op() == Ops::Insert {
                                    // Insertion
                                    source =
                                        [&source[..sl + idx2], diff.data(), &source[sl + idx2..]]
                                            .concat();
                                } else if diff.op() == Ops::Delete {
                                    // Deletion
                                    source = [
                                        &source[..sl + idx2],
                                        &source[sl + Self::x_index(&diffs, idx1 + diff.size())..],
                                    ]
                                    .concat();
                                }
                            }

                            if diff.op() != Ops::Delete {
                                idx1 += diff.size()
                            }
                        });
                    }
                }
            } else {
                // No match found.  :(
                results[x] = false;
                // Subtract the delta for this failed patch from subsequent patches.
                delta -= p.length2 - p.length1;
            }
        }

        // Strip the padding off.
        source = source[null_pad.len()..source.len() - null_pad.len()].to_vec();
        Ok((source, results))
    }

    fn patch_add_padding(&self, patches: &mut Patches) -> Vec<u8> {
        let pad_len = self.patch_margin() as usize;

        let null_pad = (1..pad_len + 1)
            .filter_map(|c| char::from_u32(c as u32).map(|c_| c_ as u8))
            .collect::<Vec<_>>();

        // Bump all the patches forward.
        patches.iter_mut().for_each(|p| {
            p.start1 += pad_len;
            p.start2 += pad_len;
        });

        // Add some padding on start of first diff.
        if let Some(first_patch) = patches.first_mut() {
            let (add_null_pad, pad_len_gt_txt_len) = if let Some(fd) = first_patch.diffs.first() {
                (fd.op() != Ops::Equal, pad_len > fd.size())
            } else {
                (true, false)
            };

            if add_null_pad {
                first_patch.diffs.insert(0, Diff::equal(&null_pad));
                first_patch.start1 -= pad_len;
                first_patch.start2 -= pad_len;
                first_patch.length1 += pad_len;
                first_patch.length2 += pad_len;
            } else if pad_len_gt_txt_len {
                // Grow first equality.
                if let Some(fd) = first_patch.diffs.first_mut() {
                    let extra_len = pad_len - fd.size();
                    fd.1 = [&null_pad[fd.size()..], fd.data()].concat();
                    first_patch.start1 -= extra_len;
                    first_patch.start2 -= extra_len;
                    first_patch.length1 += extra_len;
                    first_patch.length2 += extra_len;
                }
            }
        }

        // Add some padding on end of last diff.
        if let Some(last_patch) = patches.last_mut() {
            let (add_null_pad, pad_len_gt_txt_len) = if let Some(ld) = last_patch.diffs.last() {
                (ld.op() != Ops::Equal, pad_len > ld.size())
            } else {
                (true, false)
            };

            // Add nullPadding equality.
            if add_null_pad {
                last_patch.diffs.push(Diff::equal(&null_pad));
                last_patch.length1 += pad_len;
                last_patch.length2 += pad_len;
            } else if pad_len_gt_txt_len {
                // Grow last equality.
                if let Some(ld) = last_patch.diffs.last_mut() {
                    let extra_len = pad_len - ld.size();
                    ld.1 = [&ld.1[..], &null_pad[..extra_len]].concat();

                    last_patch.length1 += extra_len;
                    last_patch.length2 += extra_len;
                }
            }
        }

        null_pad
    }
}

// Exposed APIs
impl DiffMatchPatch {
    /// Find the differences between two texts.  Simplifies the problem by stripping any common prefix or suffix off the texts before diffing.
    /// Args:
    /// old: Old string to be diffed.
    /// new: New string to be diffed.
    /// deadline: Optional time when the diff should be complete by.  Used
    /// internally for recursive calls.  Users should set DiffTimeout instead.
    ///
    /// Returns:
    /// Vec of changes (Diff).
    pub fn diff_main(&self, old: &str, new: &str) -> Result<Vec<Diff<u8>>, crate::errors::Error> {
        self.diff_internal(
            old.as_bytes(),
            new.as_bytes(),
            self.checklines(),
            self.deadline(),
        )
    }

    ///  A diff of two unrelated texts can be filled with coincidental matches.
    /// For example, the diff of "mouse" and "sofas" is [(-1, "m"), (1, "s"), (0, "o"), (-1, "u"), (1, "fa"), (0, "s"), (-1, "e")].
    /// While this is the optimum diff, it is difficult for humans to understand. Semantic cleanup rewrites the diff, expanding it into a more intelligible format.
    /// The above example would become: [(-1, "mouse"), (1, "sofas")]. If a diff is to be human-readable, it should be passed to diff_cleanup_semantic.
    pub fn diff_cleanup_semantic(diffs: &mut Vec<Diff<u8>>) {
        Self::cleanup_semantic(diffs)
    }

    pub fn diff_cleanup_efficiency(&self, diffs: &mut Vec<Diff<u8>>) {
        self.cleanup_efficiency(diffs)
    }

    pub fn diff_levenshtein(diffs: &[Diff<u8>]) -> usize {
        let mut levenshtein = 0;
        let mut insert = 0;
        let mut delete = 0;

        diffs.iter().for_each(|diff| {
            match diff.op() {
                Ops::Insert => insert += diff.size(),
                Ops::Delete => delete += diff.size(),
                Ops::Equal => {
                    // A deletion and an insertion is one substitution.
                    levenshtein += insert.max(delete);
                    insert = 0;
                    delete = 0;
                }
            }
        });

        levenshtein += insert.max(delete);

        levenshtein
    }

    /// Takes a diff array and returns a pretty HTML sequence. This function is mainly intended as an example from which to write ones own display functions.
    pub fn diff_pretty_html(diffs: &[Diff<u8>]) -> Result<String, crate::errors::Error> {
        let mut diffs = diffs.to_vec();
        DiffMatchPatch::cleanup_semantic(&mut diffs);

        // let mut err_idx = None;
        // let mut error_bytes = vec![];

        let mut idx = 0_usize;
        let mut err_prefix = vec![];

        let mut err_start = None;

        // First pass, we'll chomp of errors in the diffs?
        // The pattern we have seen is that
        while idx < diffs.len() {
            let diff = &mut diffs[idx];

            if let Err(e) = str::from_utf8(diff.data()) {
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

        let mut is_err = false;
        let html = diffs
            .iter()
            .map(|diff| {
                let txt = match str::from_utf8(diff.data()) {
                    Ok(txt) => txt
                        .replace("&", "&amp;")
                        .replace("<", "&lt;")
                        .replace(">", "&gt;")
                        .replace("\n", "&para;<br>"),
                    Err(e) => {
                        eprintln!("{e:?}");
                        is_err = true;
                        "error".to_string()
                    }
                };

                match diff.op() {
                    Ops::Insert => format!("<ins style=\"background:#e6ffe6;\">{txt}</ins>"),
                    Ops::Delete => format!("<del style=\"background:#ffe6e6;\">{txt}</del>"),
                    Ops::Equal => format!("<span>{txt}</span>"),
                }
            })
            .collect::<Vec<_>>()
            .join("");

        if !is_err {
            Ok(html)
        } else {
            Err(crate::errors::Error::HtmlWithError(html))
        }
        // Ok(html)
    }

    pub fn match_main(&self, text: &str, pattern: &str, loc: usize) -> Option<usize> {
        self.match_internal(text.as_bytes(), pattern.as_bytes(), loc)
    }

    pub fn patch_make(&self, input: PatchInput) -> Result<Patches, crate::errors::Error> {
        let mut diff_input;
        let txt_old;
        let (txt, diffs) = match input {
            // No diffs provided, lets make our own
            PatchInput::Texts(txt1, txt2) => {
                let dmp = DiffMatchPatch::default();
                diff_input = dmp.diff_main(txt1, txt2)?;
                if diff_input.len() > 2 {
                    Self::cleanup_semantic(&mut diff_input);
                }

                (txt1.as_bytes(), &diff_input[..])
            }
            PatchInput::Diffs(diffs) => {
                // No origin string provided, compute our own.
                txt_old = Self::diff_text_old(diffs);

                (&txt_old[..], diffs)
            }
            PatchInput::TextDiffs(txt, diffs) => (txt.as_bytes(), diffs),
        };

        self.patch_make_internal(txt, diffs)
    }

    pub fn patch_to_text(patches: &Patches) -> String {
        patches.iter().map(|p| p.to_string()).collect::<String>()
    }

    pub fn patch_from_text(text: &str) -> Result<Patches, Error> {
        if text.is_empty() {
            return Ok(vec![]);
        }

        let mut text = text.as_bytes().split(|&p| p == b'\n').collect::<Vec<_>>();

        let mut patches = vec![];

        while let Some(&t) = text.first() {
            let (old_line, old_cols, new_line, new_cols) =
                if let Some(p) = Self::parse_patch_header(t) {
                    p
                } else {
                    return Err(Error::InvalidInput);
                };

            let mut patch = Patch {
                start1: old_line,
                start2: new_line,
                ..Default::default()
            };

            if let Some(old_cols) = old_cols {
                if old_cols != 0 {
                    patch.start1 -= 1;
                    patch.length1 = old_cols;
                }
            } else {
                patch.start1 -= 1;
                patch.length1 = 1;
            }

            if let Some(new_cols) = new_cols {
                if new_cols != 0 {
                    patch.start2 -= 1;
                    patch.length2 = new_cols;
                }
            } else {
                patch.start2 -= 1;
                patch.length2 = 1;
            }

            text.remove(0);

            while !text.is_empty() {
                let txt = if let Some(&s) = text.first() {
                    if !s.is_empty() {
                        s
                    } else {
                        text.remove(0);
                        continue;
                    }
                } else {
                    text.remove(0);
                    continue;
                };

                // Should never panic, already checked for `empty`
                let sign = txt.first().unwrap();

                let line = percent_decode(&txt[1..]).collect::<Vec<_>>();

                match sign {
                    b'-' => {
                        patch.diffs.push(Diff::delete(&line));
                    }
                    b'+' => {
                        patch.diffs.push(Diff::insert(&line));
                    }
                    b' ' => {
                        patch.diffs.push(Diff::equal(&line));
                    }
                    b'@' => {
                        // next patch, break
                        break;
                    }
                    _ => {
                        return Err(Error::InvalidInput);
                    }
                }

                text.remove(0);
            }

            patches.push(patch);
        }

        Ok(patches)
    }

    pub fn patch_apply(
        &self,
        patches: &Patches,
        source_txt: &str,
    ) -> Result<(String, Vec<bool>), crate::errors::Error> {
        let (str_bytes, results) = self.patch_apply_internal(patches, source_txt.as_bytes())?;

        Ok((
            String::from_utf8(str_bytes).map_err(|_| crate::errors::Error::Utf8Error)?,
            results,
        ))
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, time::Instant};

    use chrono::Utc;

    use crate::dmp::{Diff, HalfMatch, LineToChars};

    use super::{DiffMatchPatch, Ops, Patch, PatchInput};

    // const tests = [
    //     'testDiffIsDestructurable',
    //     'testDiffCleanupEfficiency',
    //     'testDiffDelta',
    // ];

    #[test]
    fn test_prefix() {
        // Detect any common prefix.
        // Null case.
        assert_eq!(
            0,
            DiffMatchPatch::common_prefix("abc".as_bytes(), "xyz".as_bytes(), false)
        );

        // Non-null case.
        assert_eq!(
            4,
            DiffMatchPatch::common_prefix("1234abcdef".as_bytes(), "1234xyz".as_bytes(), false)
        );

        // Whole case.
        assert_eq!(
            4,
            DiffMatchPatch::common_prefix("1234".as_bytes(), "1234xyz".as_bytes(), false)
        );
    }

    #[test]
    fn test_suffix() {
        // Detect any common suffix.
        // Null case.
        assert_eq!(
            0,
            DiffMatchPatch::common_prefix("abc".as_bytes(), "xyz".as_bytes(), true)
        );

        // Non-null case.
        assert_eq!(
            4,
            DiffMatchPatch::common_prefix("abcdef1234".as_bytes(), "xyz1234".as_bytes(), true)
        );

        // Whole case.
        assert_eq!(
            4,
            DiffMatchPatch::common_prefix("1234".as_bytes(), "xyz1234".as_bytes(), true)
        );
    }

    #[test]
    fn test_diff_half_match() {
        let mut dmp = DiffMatchPatch::default();

        // No match
        assert!(dmp
            .half_match("1234567890".as_bytes(), "abcdef".as_bytes())
            .is_none());
        assert!(dmp
            .half_match("12345".as_bytes(), "23".as_bytes())
            .is_none());

        // Single Match.
        assert_eq!(
            Some(HalfMatch {
                prefix_long: "12".as_bytes(),
                suffix_long: "90".as_bytes(),
                prefix_short: "a".as_bytes(),
                suffix_short: "z".as_bytes(),
                common: "345678".as_bytes()
            }),
            dmp.half_match("1234567890".as_bytes(), "a345678z".as_bytes())
        );
        assert_eq!(
            Some(HalfMatch {
                prefix_long: "a".as_bytes(),
                suffix_long: "z".as_bytes(),
                prefix_short: "12".as_bytes(),
                suffix_short: "90".as_bytes(),
                common: "345678".as_bytes()
            }),
            dmp.half_match("a345678z".as_bytes(), "1234567890".as_bytes())
        );
        assert_eq!(
            Some(HalfMatch {
                prefix_long: "abc".as_bytes(),
                suffix_long: "z".as_bytes(),
                prefix_short: "1234".as_bytes(),
                suffix_short: "0".as_bytes(),
                common: "56789".as_bytes()
            }),
            dmp.half_match("abc56789z".as_bytes(), "1234567890".as_bytes())
        );
        assert_eq!(
            Some(HalfMatch {
                prefix_long: "a".as_bytes(),
                suffix_long: "xyz".as_bytes(),
                prefix_short: "1".as_bytes(),
                suffix_short: "7890".as_bytes(),
                common: "23456".as_bytes()
            }),
            dmp.half_match("a23456xyz".as_bytes(), "1234567890".as_bytes())
        );

        // Multiple Matches.
        assert_eq!(
            Some(HalfMatch {
                prefix_long: "12123".as_bytes(),
                suffix_long: "123121".as_bytes(),
                prefix_short: "a".as_bytes(),
                suffix_short: "z".as_bytes(),
                common: "1234123451234".as_bytes()
            }),
            dmp.half_match(
                "121231234123451234123121".as_bytes(),
                "a1234123451234z".as_bytes()
            )
        );
        assert_eq!(
            Some(HalfMatch {
                prefix_long: "".as_bytes(),
                suffix_long: "-=-=-=-=-=".as_bytes(),
                prefix_short: "x".as_bytes(),
                suffix_short: "".as_bytes(),
                common: "x-=-=-=-=-=-=-=".as_bytes()
            }),
            dmp.half_match(
                "x-=-=-=-=-=-=-=-=-=-=-=-=".as_bytes(),
                "xx-=-=-=-=-=-=-=".as_bytes()
            )
        );
        assert_eq!(
            Some(HalfMatch {
                prefix_long: "-=-=-=-=-=".as_bytes(),
                suffix_long: "".as_bytes(),
                prefix_short: "".as_bytes(),
                suffix_short: "y".as_bytes(),
                common: "-=-=-=-=-=-=-=y".as_bytes()
            }),
            dmp.half_match(
                "-=-=-=-=-=-=-=-=-=-=-=-=y".as_bytes(),
                "-=-=-=-=-=-=-=yy".as_bytes()
            )
        );

        // Non-optimal halfmatch.
        // Optimal diff would be -q+x=H-i+e=lloHe+Hu=llo-Hew+y not -qHillo+x=HelloHe-w+Hulloy
        assert_eq!(
            Some(HalfMatch {
                prefix_long: "qHillo".as_bytes(),
                suffix_long: "w".as_bytes(),
                prefix_short: "x".as_bytes(),
                suffix_short: "Hulloy".as_bytes(),
                common: "HelloHe".as_bytes()
            }),
            dmp.half_match("qHilloHelloHew".as_bytes(), "xHelloHeHulloy".as_bytes())
        );

        // Optimal no halfmatch.
        dmp.timeout = None;
        assert!(dmp
            .half_match("qHilloHelloHew".as_bytes(), "xHelloHeHulloy".as_bytes())
            .is_none());
    }

    #[test]
    fn test_diff_lines_to_chars() {
        // Convert lines down to characters.
        assert_eq!(
            LineToChars {
                chars_old: vec![0_usize, 1, 0],
                chars_new: vec![1_usize, 0, 1],
                lines: vec![b"alpha\n", b"beta\n"]
            },
            DiffMatchPatch::lines_to_chars(b"alpha\nbeta\nalpha\n", b"beta\nalpha\nbeta\n")
        );
        assert_eq!(
            LineToChars {
                chars_old: vec![],
                chars_new: vec![0_usize, 1, 2, 2],
                lines: vec![b"alpha\r\n", b"beta\r\n", b"\r\n"]
            },
            DiffMatchPatch::lines_to_chars(b"", b"alpha\r\nbeta\r\n\r\n\r\n")
        );
        assert_eq!(
            LineToChars {
                chars_old: vec![0_usize],
                chars_new: vec![1_usize],
                lines: vec![b"a", b"b"]
            },
            DiffMatchPatch::lines_to_chars(b"a", b"b")
        );

        // More than 256 to reveal any 8-bit limitations.
        const TLIMIT: usize = 300;
        let linestr = (0..TLIMIT).map(|i| format!("{i}\n")).collect::<Vec<_>>();
        let linelist: Vec<&[u8]> = (0..TLIMIT).map(|i| linestr[i].as_bytes()).collect();
        let charlist = (0..TLIMIT).collect::<Vec<_>>();
        let linestr = linestr.join("");
        let res = DiffMatchPatch::lines_to_chars(linestr.as_bytes(), b"");
        let src = LineToChars {
            chars_old: charlist,
            chars_new: vec![],
            lines: linelist,
        };
        assert_eq!(res.chars_new, src.chars_new);
        assert_eq!(res.lines, src.lines);
        assert_eq!(res.chars_old, src.chars_old);
    }

    #[test]
    fn test_diff_chars_to_lines() {
        // Convert chars up to lines.
        let d1 = [0_usize, 1, 0].to_vec();
        let d2 = [1_usize, 0, 1].to_vec();
        let diffs = [Diff::equal(&d1), Diff::insert(&d2)];

        let diffs = DiffMatchPatch::chars_to_lines(&diffs, &[b"alpha\n", b"beta\n"]);

        assert_eq!(
            vec![
                Diff::equal(b"alpha\nbeta\nalpha\n"),
                Diff::insert(b"beta\nalpha\nbeta\n")
            ],
            diffs
        );

        // More than 256 to reveal any 8-bit limitations.
        const TLIMIT: usize = 300;

        let charlist = (0..TLIMIT).collect::<Vec<_>>();
        let linestr = (0..TLIMIT).map(|i| format!("{i}\n")).collect::<Vec<_>>();
        let linelist: Vec<&[u8]> = (0..TLIMIT).map(|i| linestr[i].as_bytes()).collect();

        let diffs = [Diff::delete(&charlist)];
        let diffs = DiffMatchPatch::chars_to_lines(&diffs, &linelist[..]);

        assert_eq!(vec![Diff::delete(linestr.join("").as_bytes())], diffs);

        // More than 65536 to verify any 16-bit limitation.
        const ULIMIT: usize = 10;
        let linestr = (0..ULIMIT).map(|i| format!("{i}\n")).collect::<Vec<_>>();
        let lines = linestr.join("");
        let l2c = DiffMatchPatch::lines_to_chars(lines.as_bytes(), b"");

        let diffs = [Diff::insert(&l2c.chars_old)];
        let diffs = DiffMatchPatch::chars_to_lines(&diffs, &l2c.lines);

        assert_eq!(lines.as_bytes(), diffs[0].data());
    }

    #[test]
    fn test_diff_cleanup_merge() {
        // Cleanup a messy diff.
        // Null case.
        let mut diffs = vec![];
        let test: Vec<Diff<u8>> = vec![];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // No change case
        let mut diffs = vec![Diff::equal(b"a"), Diff::delete(b"b"), Diff::insert(b"c")];
        let test = vec![Diff::equal(b"a"), Diff::delete(b"b"), Diff::insert(b"c")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Merge equalities.
        let mut diffs = vec![Diff::equal(b"a"), Diff::equal(b"b"), Diff::equal(b"c")];
        let test = vec![Diff::equal(b"abc")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Merge deletions.
        let mut diffs = vec![Diff::delete(b"a"), Diff::delete(b"b"), Diff::delete(b"c")];
        let test = vec![Diff::delete(b"abc")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Merge insertions.
        let mut diffs = vec![Diff::insert(b"a"), Diff::insert(b"b"), Diff::insert(b"c")];
        let test = vec![Diff::insert(b"abc")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Merge interweave.
        let mut diffs = vec![
            Diff::delete(b"a"),
            Diff::insert(b"b"),
            Diff::delete(b"c"),
            Diff::insert(b"d"),
            Diff::equal(b"e"),
            Diff::equal(b"f"),
        ];
        let test = vec![Diff::delete(b"ac"), Diff::insert(b"bd"), Diff::equal(b"ef")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Prefix and suffix detection.
        let mut diffs = vec![
            Diff::delete(b"a"),
            Diff::insert(b"abc"),
            Diff::delete(b"dc"),
        ];
        let test = vec![
            Diff::equal(b"a"),
            Diff::delete(b"d"),
            Diff::insert(b"b"),
            Diff::equal(b"c"),
        ];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Prefix and suffix detection with equalities.
        let mut diffs = vec![
            Diff::equal(b"x"),
            Diff::delete(b"a"),
            Diff::insert(b"abc"),
            Diff::delete(b"dc"),
            Diff::equal(b"y"),
        ];
        let test = vec![
            Diff::equal(b"xa"),
            Diff::delete(b"d"),
            Diff::insert(b"b"),
            Diff::equal(b"cy"),
        ];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Slide edit left.
        let mut diffs = vec![Diff::equal(b"a"), Diff::insert(b"ba"), Diff::equal(b"c")];
        let test = vec![Diff::insert(b"ab"), Diff::equal(b"ac")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Slide edit right
        let mut diffs = vec![Diff::equal(b"c"), Diff::insert(b"ab"), Diff::equal(b"a")];
        let test = vec![Diff::equal(b"ca"), Diff::insert(b"ba")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Slide edit left recursive.
        let mut diffs = vec![
            Diff::equal(b"a"),
            Diff::delete(b"b"),
            Diff::equal(b"c"),
            Diff::delete(b"ac"),
            Diff::equal(b"x"),
        ];
        let test = vec![Diff::delete(b"abc"), Diff::equal(b"acx")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Slide edit right recursive.
        let mut diffs = vec![
            Diff::equal(b"x"),
            Diff::delete(b"ca"),
            Diff::equal(b"c"),
            Diff::delete(b"b"),
            Diff::equal(b"a"),
        ];
        let test = vec![Diff::equal(b"xca"), Diff::delete(b"cba")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Empty merge.
        let mut diffs = vec![Diff::delete(b"b"), Diff::insert(b"ab"), Diff::equal(b"c")];
        let test = vec![Diff::insert(b"a"), Diff::equal(b"bc")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Empty equality.
        let mut diffs = vec![Diff::equal(b""), Diff::insert(b"a"), Diff::equal(b"b")];
        let test = vec![Diff::insert(b"a"), Diff::equal(b"b")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);
    }

    #[test]
    fn test_diff_cleanup_semantic_overall() {
        // Cleanup semantically trivial equalities.
        // Null case.
        let mut diffs = vec![];
        let test: Vec<Diff<u8>> = vec![];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // No elimination #1.
        let mut diffs = vec![
            Diff::delete(b"ab"),
            Diff::insert(b"cd"),
            Diff::equal(b"12"),
            Diff::delete(b"e"),
        ];
        let test: Vec<Diff<u8>> = vec![
            Diff::delete(b"ab"),
            Diff::insert(b"cd"),
            Diff::equal(b"12"),
            Diff::delete(b"e"),
        ];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // No elimination #2.
        let mut diffs = vec![
            Diff::delete(b"abc"),
            Diff::insert(b"ABC"),
            Diff::equal(b"1234"),
            Diff::delete(b"wxyz"),
        ];
        let test: Vec<Diff<u8>> = vec![
            Diff::delete(b"abc"),
            Diff::insert(b"ABC"),
            Diff::equal(b"1234"),
            Diff::delete(b"wxyz"),
        ];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // Simple elimination.
        let mut diffs = vec![Diff::delete(b"a"), Diff::equal(b"b"), Diff::delete(b"c")];
        let test: Vec<Diff<u8>> = vec![Diff::delete(b"abc"), Diff::insert(b"b")];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // Backpass elimination.
        let mut diffs = vec![
            Diff::delete(b"ab"),
            Diff::equal(b"cd"),
            Diff::delete(b"e"),
            Diff::equal(b"f"),
            Diff::insert(b"g"),
        ];
        let test: Vec<Diff<u8>> = vec![Diff::delete(b"abcdef"), Diff::insert(b"cdfg")];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // Multiple eliminations.
        let mut diffs = vec![
            Diff::insert(b"1"),
            Diff::equal(b"A"),
            Diff::delete(b"B"),
            Diff::insert(b"2"),
            Diff::equal(b"_"),
            Diff::insert(b"1"),
            Diff::equal(b"A"),
            Diff::delete(b"B"),
            Diff::insert(b"2"),
        ];
        let test: Vec<Diff<u8>> = vec![Diff::delete(b"AB_AB"), Diff::insert(b"1A2_1A2")];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // Word boundaries.
        let mut diffs = vec![
            Diff::equal(b"The c"),
            Diff::delete(b"ow and the c"),
            Diff::equal(b"at."),
        ];
        let test: Vec<Diff<u8>> = vec![
            Diff::equal(b"The "),
            Diff::delete(b"cow and the "),
            Diff::equal(b"cat."),
        ];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // No overlap elimination.
        let mut diffs = vec![Diff::delete(b"abcxx"), Diff::insert(b"xxdef")];
        let test: Vec<Diff<u8>> = vec![Diff::delete(b"abcxx"), Diff::insert(b"xxdef")];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // Overlap elimination.
        let mut diffs = vec![Diff::delete(b"abcxxx"), Diff::insert(b"xxxdef")];
        let test: Vec<Diff<u8>> = vec![
            Diff::delete(b"abc"),
            Diff::equal(b"xxx"),
            Diff::insert(b"def"),
        ];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // Reverse overlap elimination.
        let mut diffs = vec![Diff::delete(b"xxxabc"), Diff::insert(b"defxxx")];
        let test: Vec<Diff<u8>> = vec![
            Diff::insert(b"def"),
            Diff::equal(b"xxx"),
            Diff::delete(b"abc"),
        ];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // Two overlap eliminations.
        let mut diffs = vec![
            Diff::delete(b"abcd1212"),
            Diff::insert(b"1212efghi"),
            Diff::equal(b"----"),
            Diff::delete(b"A3"),
            Diff::insert(b"3BC"),
        ];
        let test: Vec<Diff<u8>> = vec![
            Diff::delete(b"abcd"),
            Diff::equal(b"1212"),
            Diff::insert(b"efghi"),
            Diff::equal(b"----"),
            Diff::delete(b"A"),
            Diff::equal(b"3"),
            Diff::insert(b"BC"),
        ];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);
    }

    #[test]
    fn test_diff_cleanup_semantic_lossless() {
        // Slide diffs to match logical boundaries.
        // Null case.
        let mut diffs: Vec<Diff<u8>> = vec![];
        let test: Vec<Diff<u8>> = vec![];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Blank lines.
        let mut diffs: Vec<Diff<u8>> = vec![
            Diff::equal(b"AAA\r\n\r\nBBB"),
            Diff::insert(b"\r\nDDD\r\n\r\nBBB"),
            Diff::equal(b"\r\nEEE"),
        ];
        let test: Vec<Diff<u8>> = vec![
            Diff::equal(b"AAA\r\n\r\n"),
            Diff::insert(b"BBB\r\nDDD\r\n\r\n"),
            Diff::equal(b"BBB\r\nEEE"),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Line boundaries.
        let mut diffs: Vec<Diff<u8>> = vec![
            Diff::equal(b"AAA\r\nBBB"),
            Diff::insert(b" DDD\r\nBBB"),
            Diff::equal(b" EEE"),
        ];
        let test: Vec<Diff<u8>> = vec![
            Diff::equal(b"AAA\r\n"),
            Diff::insert(b"BBB DDD\r\n"),
            Diff::equal(b"BBB EEE"),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Word boundaries.
        let mut diffs: Vec<Diff<u8>> = vec![
            Diff::equal(b"The c"),
            Diff::insert(b"ow and the c"),
            Diff::equal(b"at."),
        ];
        let test: Vec<Diff<u8>> = vec![
            Diff::equal(b"The "),
            Diff::insert(b"cow and the "),
            Diff::equal(b"cat."),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Alphanumeric boundaries.
        let mut diffs: Vec<Diff<u8>> = vec![
            Diff::equal(b"The-c"),
            Diff::insert(b"ow-and-the-c"),
            Diff::equal(b"at."),
        ];
        let test: Vec<Diff<u8>> = vec![
            Diff::equal(b"The-"),
            Diff::insert(b"cow-and-the-"),
            Diff::equal(b"cat."),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Hitting the start.
        let mut diffs: Vec<Diff<u8>> =
            vec![Diff::equal(b"a"), Diff::delete(b"a"), Diff::equal(b"ax")];
        let test: Vec<Diff<u8>> = vec![Diff::delete(b"a"), Diff::equal(b"aax")];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Hitting the end.
        let mut diffs: Vec<Diff<u8>> =
            vec![Diff::equal(b"xa"), Diff::delete(b"a"), Diff::equal(b"a")];
        let test: Vec<Diff<u8>> = vec![Diff::equal(b"xaa"), Diff::delete(b"a")];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Sentence boundaries.
        let mut diffs: Vec<Diff<u8>> = vec![
            Diff::equal(b"The xxx. The "),
            Diff::insert(b"zzz. The "),
            Diff::equal(b"yyy."),
        ];
        let test: Vec<Diff<u8>> = vec![
            Diff::equal(b"The xxx."),
            Diff::insert(b" The zzz."),
            Diff::equal(b" The yyy."),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);
    }

    #[test]
    fn test_diff_levenshtein() {
        let diffs = vec![
            Diff::delete(b"abc"),
            Diff::insert(b"1234"),
            Diff::equal(b"xyz"),
        ];
        assert_eq!(4, DiffMatchPatch::diff_levenshtein(&diffs));

        let diffs = vec![
            Diff::equal(b"xyz"),
            Diff::delete(b"abc"),
            Diff::insert(b"1234"),
        ];
        assert_eq!(4, DiffMatchPatch::diff_levenshtein(&diffs));

        let diffs = vec![
            Diff::delete(b"abc"),
            Diff::equal(b"xyz"),
            Diff::insert(b"1234"),
        ];
        assert_eq!(7, DiffMatchPatch::diff_levenshtein(&diffs));
    }

    #[test]
    fn test_diff_x_index() {
        // Translate a location in text1 to text2.
        let diffs = vec![
            Diff::delete(b"a"),
            Diff::insert(b"1234"),
            Diff::equal(b"xyz"),
        ];
        assert_eq!(5, DiffMatchPatch::x_index(&diffs, 2));

        let diffs = vec![
            Diff::equal(b"a"),
            Diff::delete(b"1234"),
            Diff::equal(b"xyz"),
        ];
        assert_eq!(1, DiffMatchPatch::x_index(&diffs, 3));
    }

    #[test]
    fn test_diff_common_overlap() {
        // Detect any suffix/prefix overlap.
        // Null case
        assert_eq!(0, DiffMatchPatch::common_overlap(b"", b"abcd"));

        // Whole case.
        assert_eq!(3, DiffMatchPatch::common_overlap(b"abc", b"abcd"));

        // No overlap.
        assert_eq!(0, DiffMatchPatch::common_overlap(b"123456", b"abcd"));

        // Overlap.
        assert_eq!(3, DiffMatchPatch::common_overlap(b"123456xxx", b"xxxabcd"));

        // Unicode.
        // Some overly clever languages (C#) may treat ligatures as equal to their
        // component letters.  E.g. U+FB01 == 'fi'
        assert_eq!(
            0,
            DiffMatchPatch::common_overlap(b"fi", "\u{7FFF}".as_bytes())
        );

        // Complete overlap
        assert_eq!(
            6,
            DiffMatchPatch::common_overlap("❤️".as_bytes(), "❤️".as_bytes())
        );

        // Partial unicode overlap
        assert_eq!(
            0,
            DiffMatchPatch::common_overlap("ஆ".as_bytes(), "இ".as_bytes())
        );
        assert_eq!(
            3,
            DiffMatchPatch::common_overlap("ஆஇ".as_bytes(), "இ".as_bytes())
        );
    }

    #[test]
    fn test_diff_bisect() -> Result<(), crate::errors::Error> {
        let mut dmp = DiffMatchPatch::default();

        // Normal.
        // Since the resulting diff hasn't been normalized, it would be ok if
        // the insertion and deletion pairs are swapped.
        // If the order changes, tweak this test as required.
        assert_eq!(
            vec![
                Diff::delete(b"c"),
                Diff::insert(b"m"),
                Diff::equal(b"a"),
                Diff::delete(b"t"),
                Diff::insert(b"p")
            ],
            dmp.bisect(b"cat", b"map", None)?
        );

        // Timeout.
        dmp.timeout = Some(0);
        let deadline = dmp.deadline();
        assert_eq!(
            vec![Diff::delete(b"cat"), Diff::insert(b"map"),],
            dmp.bisect(b"cat", b"map", deadline)?
        );

        Ok(())
    }

    #[test]
    fn test_diff_pretty_html() -> Result<(), crate::errors::Error> {
        // Basic
        let diffs = [
            Diff::equal(b"a\n"),
            Diff::delete(b"<B>b</B>"),
            Diff::insert(b"c&d"),
        ];
        assert_eq!("<span>a&para;<br></span><del style=\"background:#ffe6e6;\">&lt;B&gt;b&lt;/B&gt;</del><ins style=\"background:#e6ffe6;\">c&amp;d</ins>", DiffMatchPatch::diff_pretty_html(&diffs)?);

        // Monkey busiess around Emoticons and extended utf-8 🤪🤩🤔
        // This gave me a lot of heart-burn
        let dmp = DiffMatchPatch::default();

        // Case 1. Two similar emoticons
        // In bytes representation, these would have the last u8 different
        // Which means the the diff should an equality block of 3 bytes folloed by insert and delete
        let old = "🤪"; // [240, 159, 164, 170]
        let new = "🤔"; // [240, 159, 164, 148]
        let diffs = dmp.diff_main(old, new)?;
        assert_eq!(
            "<span></span><del style=\"background:#ffe6e6;\">🤪</del><ins style=\"background:#e6ffe6;\">🤔</ins>",
            DiffMatchPatch::diff_pretty_html(&diffs)?
        );

        // Now Case 1. but with some text before and after
        let old = "I'm puzzled🤪 or am I?";
        let new = "I'm puzzled🤔 or thinking I guess!";
        let diffs = dmp.diff_main(old, new)?;
        assert_eq!(
            "<span>I'm puzzled</span><del style=\"background:#ffe6e6;\">🤪</del><ins style=\"background:#e6ffe6;\">🤔</ins><span> or </span><del style=\"background:#ffe6e6;\">am I?</del><ins style=\"background:#e6ffe6;\">thinking I guess!</ins>",
            DiffMatchPatch::diff_pretty_html(&diffs)?
        );

        // Case 2. Emoticons with the third position different
        let old = "🍊"; // [240, 159, 141, 138]
        let new = "🌊"; // [240, 159, 140, 138]
        let diffs = dmp.diff_main(old, new)?;
        assert_eq!(
            "<span></span><del style=\"background:#ffe6e6;\">🍊</del><ins style=\"background:#e6ffe6;\">🌊</ins>",
            DiffMatchPatch::diff_pretty_html(&diffs)?
        );

        // Now Case 2. but with some text, lets complicate this
        let old = "🍊, aah orange is the new black!"; // [240, 159, 141, 138]
        let new = "Aah orange!🌊is the new 🌊"; // [240, 159, 140, 138]
        let diffs = dmp.diff_main(old, new)?;
        assert_eq!(
            "<del style=\"background:#ffe6e6;\">🍊, a</del><ins style=\"background:#e6ffe6;\">A</ins><span>ah orange</span><del style=\"background:#ffe6e6;\"> </del><ins style=\"background:#e6ffe6;\">!🌊</ins><span>is the new </span><del style=\"background:#ffe6e6;\">black!</del><ins style=\"background:#e6ffe6;\">🌊</ins>",
            DiffMatchPatch::diff_pretty_html(&diffs)?
        );

        // Case 3. with second and third different, but lets complicate this with an equality
        let old = "𠌊"; // [240, 160, 140, 138]
        let new = "𖠊"; // [240, 150, 160, 138]
        let diffs = dmp.diff_main(old, new)?;
        assert_eq!(
            "<span></span><ins style=\"background:#e6ffe6;\">𖠊</ins><del style=\"background:#ffe6e6;\">𠌊</del>",
            DiffMatchPatch::diff_pretty_html(&diffs)?
        );

        // Case 3. but let there be a swap
        let old = "𞠄"; // [240, 158, 160, 132]
        let new = std::str::from_utf8(&[240, 160, 158, 132]).unwrap(); // basically an undefined element `𠞄`. Should still work
        let diffs = dmp.diff_main(old, new)?;
        assert_eq!(
            "<span></span><del style=\"background:#ffe6e6;\">𞠄</del><ins style=\"background:#e6ffe6;\">𠞄</ins>",
            DiffMatchPatch::diff_pretty_html(&diffs)?
        );

        // Case 4. swap at the last 2 positions
        let old = "🍌"; // [240, 159, 141, 140] -- FINALLY A BANANA
        let new = "🌍"; // [240, 159, 140, 141] -- interesting revelation - last 2 bytes swapped and 🍌 becomes 🌍. Guess the world is going `Bananas!!`
        let diffs = dmp.diff_main(old, new)?;
        assert_eq!(
            "<span></span><del style=\"background:#ffe6e6;\">🍌</del><ins style=\"background:#e6ffe6;\">🌍</ins>",
            DiffMatchPatch::diff_pretty_html(&diffs)?
        );

        // Let's do this with a slightly longish string
        let old = "Now, let's explore some emotional extremes 🌊.\nWe've got your ecstatic face 🤩, your devastated face 😭, and your utterly confused face 🤯. But that's not all! 🤔 We've also got some subtle emotions like 😐, 🙃, and 👀.";
        let new = "Let's start with some basics 😊.\nWe've got your standard smiley face 🙂, your sad face ☹️, and your angry face 😠. But wait, there's more! 🤩 We've also got some more complex emotions like 😍, 🤤, and 🚀. And let's not forget about the classics: 😉, 👍, and 👏.";
        let diffs = dmp.diff_main(old, new)?;

        assert_eq!(
            "<del style=\"background:#ffe6e6;\">Now, let's explore some emotional extreme</del><ins style=\"background:#e6ffe6;\">Let's start with some basic</ins><span>s </span><del style=\"background:#ffe6e6;\">🌊</del><ins style=\"background:#e6ffe6;\">😊</ins><span>.&para;<br>We've got your </span><del style=\"background:#ffe6e6;\">ec</del><span>sta</span><del style=\"background:#ffe6e6;\">tic</del><ins style=\"background:#e6ffe6;\">ndard smiley</ins><span> face </span><del style=\"background:#ffe6e6;\">🤩</del><ins style=\"background:#e6ffe6;\">🙂</ins><span>, your </span><del style=\"background:#ffe6e6;\">devastate</del><ins style=\"background:#e6ffe6;\">sa</ins><span>d face </span><del style=\"background:#ffe6e6;\">😭</del><ins style=\"background:#e6ffe6;\">☹️</ins><span>, and your </span><del style=\"background:#ffe6e6;\">utterly confused</del><ins style=\"background:#e6ffe6;\">angry</ins><span> face </span><del style=\"background:#ffe6e6;\">🤯</del><ins style=\"background:#e6ffe6;\">😠</ins><span>. But </span><del style=\"background:#ffe6e6;\">that's not all</del><ins style=\"background:#e6ffe6;\">wait, there's more</ins><span>! </span><del style=\"background:#ffe6e6;\">🤔</del><ins style=\"background:#e6ffe6;\">🤩</ins><span> We've also got some </span><del style=\"background:#ffe6e6;\">subt</del><ins style=\"background:#e6ffe6;\">more comp</ins><span>le</span><ins style=\"background:#e6ffe6;\">x</ins><span> emotions like </span><del style=\"background:#ffe6e6;\">😐</del><ins style=\"background:#e6ffe6;\">😍, 🤤, and 🚀. And let's not forget about the classics: 😉</ins><span>, </span><del style=\"background:#ffe6e6;\">🙃</del><ins style=\"background:#e6ffe6;\">👍</ins><span>, and </span><del style=\"background:#ffe6e6;\">👀</del><ins style=\"background:#e6ffe6;\">👏</ins><span>.</span>",
            DiffMatchPatch::diff_pretty_html(&diffs)?
        );

        Ok(())
    }

    #[test]
    fn test_diff_main() -> Result<(), crate::errors::Error> {
        let mut dmp = DiffMatchPatch::default();

        // Perform a trivial diff.
        // Null case.
        // assert!(dmp.diff_main("", "")?.is_empty());

        // // Equality
        // assert_eq!(vec![Diff::equal(b"abc")], dmp.diff_main("abc", "abc")?);

        // // Simple insert
        // assert_eq!(
        //     vec![Diff::equal(b"ab"), Diff::insert(b"123"), Diff::equal(b"c")],
        //     dmp.diff_main("abc", "ab123c")?
        // );

        // // Simple delete
        // assert_eq!(
        //     vec![Diff::equal(b"a"), Diff::delete(b"123"), Diff::equal(b"bc")],
        //     dmp.diff_main("a123bc", "abc")?
        // );

        // // Two insertions
        // assert_eq!(
        //     vec![
        //         Diff::equal(b"a"),
        //         Diff::insert(b"123"),
        //         Diff::equal(b"b"),
        //         Diff::insert(b"456"),
        //         Diff::equal(b"c"),
        //     ],
        //     dmp.diff_main("abc", "a123b456c")?
        // );

        // // Two deletions.
        // assert_eq!(
        //     vec![
        //         Diff::equal(b"a"),
        //         Diff::delete(b"123"),
        //         Diff::equal(b"b"),
        //         Diff::delete(b"456"),
        //         Diff::equal(b"c"),
        //     ],
        //     dmp.diff_main("a123b456c", "abc")?
        // );

        // // Perform a real diff.
        // // Switch off the timeout.
        // dmp.timeout = None;
        // // Simple cases.
        // assert_eq!(
        //     vec![Diff::delete(b"a"), Diff::insert(b"b"),],
        //     dmp.diff_main("a", "b")?
        // );

        // assert_eq!(
        //     vec![
        //         Diff::delete(b"Apple"),
        //         Diff::insert(b"Banana"),
        //         Diff::equal(b"s are a"),
        //         Diff::insert(b"lso"),
        //         Diff::equal(b" fruit.")
        //     ],
        //     dmp.diff_main("Apples are a fruit.", "Bananas are also fruit.")?
        // );

        // assert_eq!(
        //     vec![
        //         Diff::delete(b"a"),
        //         Diff::insert("\u{0680}".as_bytes()),
        //         Diff::equal(b"x"),
        //         Diff::delete(b"\t"),
        //         Diff::insert(b"\0")
        //     ],
        //     dmp.diff_main("ax\t", "\u{0680}x\0")?
        // );

        // // Overlaps.
        // assert_eq!(
        //     vec![
        //         Diff::delete(b"1"),
        //         Diff::equal(b"a"),
        //         Diff::delete(b"y"),
        //         Diff::equal(b"b"),
        //         Diff::delete(b"2"),
        //         Diff::insert(b"xab"),
        //     ],
        //     dmp.diff_main("1ayb2", "abxab")?
        // );

        // assert_eq!(
        //     vec![
        //         Diff::insert(b"xaxcx"),
        //         Diff::equal(b"abc"),
        //         Diff::delete(b"y"),
        //     ],
        //     dmp.diff_main("abcy", "xaxcxabc")?
        // );

        // assert_eq!(
        //     vec![
        //         Diff::delete(b"ABCD"),
        //         Diff::equal(b"a"),
        //         Diff::delete(b"="),
        //         Diff::insert(b"-"),
        //         Diff::equal(b"bcd"),
        //         Diff::delete(b"="),
        //         Diff::insert(b"-"),
        //         Diff::equal(b"efghijklmnopqrs"),
        //         Diff::delete(b"EFGHIJKLMNOefg"),
        //     ],
        //     dmp.diff_main(
        //         "ABCDa=bcd=efghijklmnopqrsEFGHIJKLMNOefg",
        //         "a-bcd-efghijklmnopqrs"
        //     )?
        // );

        // // Large equality.
        // assert_eq!(
        //     vec![
        //         Diff::insert(b" "),
        //         Diff::equal(b"a"),
        //         Diff::insert(b"nd"),
        //         Diff::equal(b" [[Hepatopancreatic]]"),
        //         Diff::delete(b" and [[New"),
        //     ],
        //     dmp.diff_main(
        //         "a [[Hepatopancreatic]] and [[New",
        //         " and [[Hepatopancreatic]]"
        //     )?
        // );

        // // Timeout.
        // const LOW_TIMEOUT: u32 = 100;
        // dmp.set_timeout(Some(LOW_TIMEOUT));
        // let a = vec!["`Twas brillig, and the slithy toves\nDid gyre and gimble in the wabe:\nAll mimsy were the borogoves,\nAnd the mome raths outgrabe.\n"; 2048].join("");
        // let b = vec!["I am the very model of a modern major general,\nI\'ve information vegetable, animal, and mineral,\nI know the kings of England, and I quote the fights historical,\nFrom Marathon to Waterloo, in order categorical.\n"; 2048].join("");

        // let start = Utc::now().time();
        // dmp.diff_main(&a, &b)?;
        // let end = Utc::now().time();
        // // Test that we took at least the timeout period (+ 5ms being generous).
        // assert!((end - start).num_milliseconds() <= LOW_TIMEOUT as i64 + 5);

        // // Test the linemode speedup.
        // // Must be long to pass the 100 char cutoff.
        // // Simple line-mode.
        // dmp.timeout = Some(1000);
        // let a =  "12345678901234567890123456789 0123456 78901234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n";
        // let b = "abcdefghij abcdefghij abcdefghij abcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\n";
        // dmp.checklines = false;
        // //
        // let res_no_lm = dmp.diff_main(a, b)?;
        // // let no_lm = Instant::now() - start;
        // dmp.checklines = true;
        // // let start = Instant::now();
        // let res_yes_lm = dmp.diff_main(a, b)?;
        // // let yes_lm = Instant::now() - start;

        // // Now, we'll run 2 checks - one for result equality, two for speedup
        // assert_eq!(res_no_lm, res_yes_lm);

        // // Single line-mode.
        // let a = "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890";
        // let b = "abcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghij";
        // dmp.checklines = false;
        // let yes_lm = dmp.diff_main(a, b)?;
        // dmp.checklines = true;
        // let no_lm = dmp.diff_main(a, b)?;
        // assert_eq!(no_lm, yes_lm);

        // // Overlap line-mode.
        // let a = "1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n";
        // let b = "abcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n";
        // dmp.checklines = false;
        // let start = Instant::now();
        // let no_lm = dmp.diff_main(a, b)?;
        // let no_lm_t = Instant::now() - start;
        // dmp.checklines = true;
        // let start = Instant::now();
        // let yes_lm = dmp.diff_main(a, b)?;
        // let yes_lm_t = Instant::now() - start;
        // assert_eq!(rebuild_text(&yes_lm[..])?, rebuild_text(&no_lm[..])?);

        // assert!(no_lm_t > yes_lm_t);

        let dmp = DiffMatchPatch::default();
        let old = std::fs::read_to_string("testdata/txt_old.txt").unwrap();
        let new = std::fs::read_to_string("testdata/txt_new.txt").unwrap();
        assert!(dmp.diff_main(&old, &new).is_ok());
        Ok(())
    }

    // #[test]
    // fn test_diff_delta() {
    // let diffs = vec![
    //     Diff::equal(b"jump"),
    //     Diff::delete(b"s"),
    //     Diff::insert(b"ed"),
    //     Diff::equal(b" over "),
    //     Diff::delete(b"the"),
    //     Diff::insert(b"a"),
    //     Diff::equal(b" lazy"),
    //     Diff::insert(b"old dog"),
    // ];
    // assert_eq!(
    //     b"jumps over the lazy".to_vec(),
    //     DiffMatchPatch::diff_text_old(&diffs)
    // );

    // var delta = dmp.diff_toDelta(diffs);
    // assertEquals('=4\t-1\t+ed\t=6\t-3\t+a\t=5\t+old dog', delta);

    // // Convert delta string into a diff.
    // assertEquivalent(diffs, dmp.diff_fromDelta(text1, delta));

    // // Generates error (19 != 20).
    // try {
    //     dmp.diff_fromDelta(text1 + 'x', delta);
    //     assertEquals(Error, null);
    // } catch (e) {
    //     // Exception expected.
    // }

    // // Generates error (19 != 18).
    // try {
    //     dmp.diff_fromDelta(text1.substring(1), delta);
    //     assertEquals(Error, null);
    // } catch (e) {
    //     // Exception expected.
    // }

    // // Generates error (%c3%xy invalid Unicode).
    // try {
    //     dmp.diff_fromDelta('', '+%c3%xy');
    //     assertEquals(Error, null);
    // } catch (e) {
    //     // Exception expected.
    // }

    // // Test deltas with special characters.
    // diffs = [[DIFF_EQUAL, '\u0680 \x00 \t %'], [DIFF_DELETE, '\u0681 \x01 \n ^'], [DIFF_INSERT, '\u0682 \x02 \\ |']];
    // text1 = dmp.diff_text1(diffs);
    // assertEquals('\u0680 \x00 \t %\u0681 \x01 \n ^', text1);

    // delta = dmp.diff_toDelta(diffs);
    // assertEquals('=7\t-7\t+%DA%82 %02 %5C %7C', delta);

    // // Convert delta string into a diff.
    // assertEquivalent(diffs, dmp.diff_fromDelta(text1, delta));

    // diffs = [[DIFF_EQUAL, '\ud83d\ude4b\ud83d'], [DIFF_INSERT, '\ude4c\ud83d'], [DIFF_EQUAL, '\ude4b']];
    // try {
    //     delta = dmp.diff_toDelta(diffs);
    //     assertEquals('=2\t+%F0%9F%99%8C\t=2', delta);
    // } catch ( e ) {
    //     assertEquals(false, true);
    // }

    // (function(){
    //     const originalText = `U+1F17x	🅰️	🅱️		🅾️	🅿️ safhawifhkw
    //     U+1F18x															🆎
    //     0	1	2	3	4	5	6	7	8	9	A	B	C	D	E	F
    //     U+1F19x		🆑	🆒	🆓	🆔	🆕	🆖	🆗	🆘	🆙	🆚
    //     U+1F20x		🈁	🈂️							sfss.,_||saavvvbbds
    //     U+1F21x	🈚
    //     U+1F22x			🈯
    //     U+1F23x			🈲	🈳	🈴	🈵	🈶	🈷️	🈸	🈹	🈺
    //     U+1F25x	🉐	🉑
    //     U+1F30x	🌀	🌁	🌂	🌃	🌄	🌅	🌆	🌇	🌈	🌉	🌊	🌋	🌌	🌍	🌎	🌏
    //     U+1F31x	🌐	🌑	🌒	🌓	🌔	🌕	🌖	🌗	🌘	🌙	🌚	🌛	🌜	🌝	🌞	`;

    //     // applies some random edits to string and returns new, edited string
    //     function applyRandomTextEdit(text) {
    //     let textArr = [...text];
    //     let r = Math.random();
    //     if(r < 1/3) { // swap
    //     let swapCount = Math.floor(Math.random()*5);
    //         for(let i = 0; i < swapCount; i++) {
    //         let swapPos1 = Math.floor(Math.random()*textArr.length);
    //         let swapPos2 = Math.floor(Math.random()*textArr.length);
    //         let char1 = textArr[swapPos1];
    //         let char2 = textArr[swapPos2];
    //         textArr[swapPos1] = char2;
    //         textArr[swapPos2] = char1;
    //         }
    //     } else if(r < 2/3) { // remove
    //         let removeCount = Math.floor(Math.random()*5);
    //         for(let i = 0; i < removeCount; i++) {
    //         let removePos = Math.floor(Math.random()*textArr.length);
    //         textArr[removePos] = "";
    //         }
    //     } else { // add
    //         let addCount = Math.floor(Math.random()*5);
    //         for(let i = 0; i < addCount; i++) {
    //         let addPos = Math.floor(Math.random()*textArr.length);
    //         let addFromPos = Math.floor(Math.random()*textArr.length);
    //         textArr[addPos] = textArr[addPos] + textArr[addFromPos];
    //         }
    //     }
    //     return textArr.join("");
    //     }

    //     for(let i = 0; i < 1000; i++) {
    //     const newText = applyRandomTextEdit(originalText);
    //     dmp.diff_toDelta(dmp.diff_main(originalText, newText));
    //     }
    // })();

    // // Unicode - splitting surrogates
    // try {
    //     assertEquivalent(
    //     dmp.diff_toDelta([[DIFF_INSERT,'\ud83c\udd71'], [DIFF_EQUAL, '\ud83c\udd70\ud83c\udd71']]),
    //     dmp.diff_toDelta(dmp.diff_main('\ud83c\udd70\ud83c\udd71', '\ud83c\udd71\ud83c\udd70\ud83c\udd71'))
    //     );
    // } catch ( e ) {
    //     assertEquals('Inserting similar surrogate pair at beginning', 'crashed');
    // }

    // try {
    //     assertEquivalent(
    //     dmp.diff_toDelta([[DIFF_EQUAL,'\ud83c\udd70'], [DIFF_INSERT, '\ud83c\udd70'], [DIFF_EQUAL, '\ud83c\udd71']]),
    //     dmp.diff_toDelta(dmp.diff_main('\ud83c\udd70\ud83c\udd71', '\ud83c\udd70\ud83c\udd70\ud83c\udd71'))
    //     );
    // } catch ( e ) {
    //     assertEquals('Inserting similar surrogate pair in the middle', 'crashed');
    // }

    // try {
    //     assertEquivalent(
    //     dmp.diff_toDelta([[DIFF_DELETE,'\ud83c\udd71'], [DIFF_EQUAL, '\ud83c\udd70\ud83c\udd71']]),
    //     dmp.diff_toDelta(dmp.diff_main('\ud83c\udd71\ud83c\udd70\ud83c\udd71', '\ud83c\udd70\ud83c\udd71'))
    //     );
    // } catch ( e ) {
    //     assertEquals('Deleting similar surrogate pair at the beginning', 'crashed');
    // }

    // try {
    //     assertEquivalent(
    //     dmp.diff_toDelta([[DIFF_EQUAL, '\ud83c\udd70'], [DIFF_DELETE,'\ud83c\udd72'], [DIFF_EQUAL, '\ud83c\udd71']]),
    //     dmp.diff_toDelta(dmp.diff_main('\ud83c\udd70\ud83c\udd72\ud83c\udd71', '\ud83c\udd70\ud83c\udd71'))
    //     );
    // } catch ( e ) {
    //     assertEquals('Deleting similar surrogate pair in the middle', 'crashed');
    // }

    // try {
    //     assertEquivalent(
    //     dmp.diff_toDelta([[DIFF_DELETE, '\ud83c\udd70'], [DIFF_INSERT, '\ud83c\udd71']]),
    //     dmp.diff_toDelta([[DIFF_EQUAL, '\ud83c'], [DIFF_DELETE, '\udd70'], [DIFF_INSERT, '\udd71']]),
    //     );
    // } catch ( e ) {
    //     assertEquals('Swap surrogate pair', 'crashed');
    // }

    // try {
    //     assertEquivalent(
    //     dmp.diff_toDelta([[DIFF_INSERT, '\ud83c\udd70'], [DIFF_DELETE, '\ud83c\udd71']]),
    //     dmp.diff_toDelta([[DIFF_EQUAL, '\ud83c'], [DIFF_INSERT, '\udd70'], [DIFF_DELETE, '\udd71']]),
    //     );
    // } catch ( e ) {
    //     assertEquals('Swap surrogate pair', 'crashed');
    // }

    // // Empty diff groups
    // assertEquivalent(
    //     dmp.diff_toDelta([[DIFF_EQUAL, 'abcdef'], [DIFF_DELETE, ''], [DIFF_INSERT, 'ghijk']]),
    //     dmp.diff_toDelta([[DIFF_EQUAL, 'abcdef'], [DIFF_INSERT, 'ghijk']]),
    // );

    // // Different versions of the library may have created deltas with
    // // half of a surrogate pair encoded as if it were valid UTF-8
    // try {
    //     assertEquivalent(
    //     dmp.diff_toDelta(dmp.diff_fromDelta('\ud83c\udd70', '-2\t+%F0%9F%85%B1')),
    //     dmp.diff_toDelta(dmp.diff_fromDelta('\ud83c\udd70', '=1\t-1\t+%ED%B5%B1'))
    //     );
    // } catch ( e ) {
    //     assertEquals('Decode UTF8-encoded surrogate half', 'crashed');
    // }

    // // Verify pool of unchanged characters.
    // diffs = [[DIFF_INSERT, 'A-Z a-z 0-9 - _ . ! ~ * \' ( ) ; / ? : @ & = + $ , # ']];
    // var text2 = dmp.diff_text2(diffs);
    // assertEquals('A-Z a-z 0-9 - _ . ! ~ * \' ( ) ; / ? : @ & = + $ , # ', text2);

    // delta = dmp.diff_toDelta(diffs);
    // assertEquals('+A-Z a-z 0-9 - _ . ! ~ * \' ( ) ; / ? : @ & = + $ , # ', delta);

    // // Convert delta string into a diff.
    // assertEquivalent(diffs, dmp.diff_fromDelta('', delta));

    // // 160 kb string.
    // var a = 'abcdefghij';
    // for (var i = 0; i < 14; i++) {
    //     a += a;
    // }
    // diffs = [[DIFF_INSERT, a]];
    // delta = dmp.diff_toDelta(diffs);
    // assertEquals('+' + a, delta);

    // // Convert delta string into a diff.
    // assertEquivalent(diffs, dmp.diff_fromDelta('', delta));
    // }

    // Helper to construct the two texts which made up the diff originally.
    fn rebuild_text(diffs: &[Diff<u8>]) -> Result<(String, String), crate::errors::Error> {
        let mut txt1 = vec![];
        let mut txt2 = vec![];

        diffs.iter().for_each(|d| {
            if d.op() != Ops::Insert {
                txt1.push(d.data());
            }

            if d.op() != Ops::Delete {
                txt2.push(d.data());
            }
        });

        Ok((
            String::from_utf8(txt1.concat()).map_err(|_| crate::errors::Error::Utf8Error)?,
            String::from_utf8(txt2.concat()).map_err(|_| crate::errors::Error::Utf8Error)?,
        ))
    }

    #[test]
    fn test_patch_obj() {
        let p = Patch {
            start1: 20,
            start2: 21,
            length1: 18,
            length2: 17,
            diffs: vec![
                Diff::equal(b"jump"),
                Diff::delete(b"s"),
                Diff::insert(b"ed"),
                Diff::equal(b" over "),
                Diff::delete(b"the"),
                Diff::insert(b"a"),
                Diff::equal(b"\nlaz"),
            ],
        };
        assert_eq!(
            "@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n %0Alaz\n",
            p.to_string()
        );
    }

    #[test]
    fn test_patch_add_context() -> Result<(), crate::errors::Error> {
        let dmp = DiffMatchPatch::default();

        let mut ps = DiffMatchPatch::patch_from_text("@@ -21,4 +21,10 @@\n-jump\n+somersault\n")?;
        let p = ps.first_mut().unwrap();
        dmp.patch_add_context(p, b"The quick brown fox jumps over the lazy dog.");
        assert_eq!(
            "@@ -17,12 +17,18 @@\n fox \n-jump\n+somersault\n s ov\n",
            p.to_string()
        );

        // Same, but not enough trailing context.
        let mut ps = DiffMatchPatch::patch_from_text("@@ -21,4 +21,10 @@\n-jump\n+somersault\n")?;
        let p = ps.first_mut().unwrap();
        dmp.patch_add_context(p, b"The quick brown fox jumps.");
        assert_eq!(
            "@@ -17,10 +17,16 @@\n fox \n-jump\n+somersault\n s.\n",
            p.to_string()
        );

        // Same, but not enough leading context.
        let mut ps = DiffMatchPatch::patch_from_text("@@ -3 +3,2 @@\n-e\n+at\n")?;
        let p = ps.first_mut().unwrap();
        dmp.patch_add_context(p, b"The quick brown fox jumps.");
        assert_eq!("@@ -1,7 +1,8 @@\n Th\n-e\n+at\n  qui\n", p.to_string());

        // Same, but with ambiguity.
        let mut ps = DiffMatchPatch::patch_from_text("@@ -3 +3,2 @@\n-e\n+at\n")?;
        let p = ps.first_mut().unwrap();
        dmp.patch_add_context(
            p,
            b"The quick brown fox jumps.  The quick brown fox crashes.",
        );
        assert_eq!(
            "@@ -1,27 +1,28 @@\n Th\n-e\n+at\n  quick brown fox jumps. \n",
            p.to_string()
        );

        Ok(())
    }

    #[test]
    fn test_patch_from_text() -> Result<(), crate::errors::Error> {
        assert!(DiffMatchPatch::patch_from_text("")?.is_empty());

        assert_eq!(
            "@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n %0Alaz\n",
            DiffMatchPatch::patch_from_text(
                "@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n %0Alaz\n"
            )?[0]
                .to_string()
        );

        assert_eq!(
            "@@ -1 +1 @@\n-a\n+b\n",
            DiffMatchPatch::patch_from_text("@@ -1 +1 @@\n-a\n+b\n")?[0].to_string()
        );

        assert_eq!(
            "@@ -1,3 +0,0 @@\n-abc\n",
            DiffMatchPatch::patch_from_text("@@ -1,3 +0,0 @@\n-abc\n")?[0].to_string()
        );

        assert_eq!(
            "@@ -0,0 +1,3 @@\n+abc\n",
            DiffMatchPatch::patch_from_text("@@ -0,0 +1,3 @@\n+abc\n")?[0].to_string()
        );

        // Generates error.
        assert!(DiffMatchPatch::patch_from_text("Bad\nPatch\n").is_err());

        Ok(())
    }

    #[test]
    fn patch_to_text() -> Result<(), crate::errors::Error> {
        let strp = "@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n  laz\n";
        let patches = DiffMatchPatch::patch_from_text(strp)?;
        assert_eq!(strp, DiffMatchPatch::patch_to_text(&patches));

        let strp = "@@ -1,9 +1,9 @@\n-f\n+F\n oo+fooba\n@@ -7,9 +7,9 @@\n obar\n-,\n+.\n  tes\n";
        let patches = DiffMatchPatch::patch_from_text(strp)?;
        assert_eq!(strp, DiffMatchPatch::patch_to_text(&patches));

        Ok(())
    }

    #[test]
    fn test_patch_make() -> Result<(), crate::errors::Error> {
        let dmp = DiffMatchPatch::default();
        let patches = dmp.patch_make(super::PatchInput::Texts("", ""))?;
        assert!(patches.is_empty());

        let txt1 = "The quick brown fox jumps over the lazy dog.";
        let txt2 = "That quick brown fox jumped over a lazy dog.";

        // The second patch must be "-21,17 +21,18", not "-22,17 +21,18" due to rolling context.
        let patches = dmp.patch_make(crate::dmp::PatchInput::Texts(txt2, txt1))?;
        assert_eq!("@@ -1,8 +1,7 @@\n Th\n-at\n+e\n  qui\n@@ -21,17 +21,18 @@\n jump\n-ed\n+s\n  over \n-a\n+the\n  laz\n", DiffMatchPatch::patch_to_text(&patches));

        // Text1+Text2 inputs.
        let patches = dmp.patch_make(crate::dmp::PatchInput::Texts(txt1, txt2))?;
        assert_eq!("@@ -1,11 +1,12 @@\n Th\n-e\n+at\n  quick b\n@@ -22,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n  laz\n", DiffMatchPatch::patch_to_text(&patches));

        // Diff input.
        // var diffs = dmp.diff_main(text1, text2, false);
        let diffs = dmp.diff_main(txt1, txt2)?;
        let patches = dmp.patch_make(crate::dmp::PatchInput::Diffs(&diffs[..]))?;
        assert_eq!("@@ -1,11 +1,12 @@\n Th\n-e\n+at\n  quick b\n@@ -22,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n  laz\n", DiffMatchPatch::patch_to_text(&patches));

        // Text1+Diff inputs.
        let patches = dmp.patch_make(crate::dmp::PatchInput::TextDiffs(txt1, &diffs[..]))?;
        assert_eq!("@@ -1,11 +1,12 @@\n Th\n-e\n+at\n  quick b\n@@ -22,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n  laz\n", DiffMatchPatch::patch_to_text(&patches));

        // Character encoding.
        let patches = dmp.patch_make(crate::dmp::PatchInput::Texts(
            "`1234567890-=[]\\;',./",
            "~!@#$%^&*()_+{}|:\"<>?",
        ))?;
        assert_eq!(
            percent_encoding::percent_decode(b"@@ -1,21 +1,21 @@\n-%601234567890-=%5B%5D%5C;',./\n+~!@#$%25%5E&*()_+%7B%7D%7C:%22%3C%3E?\n").decode_utf8().unwrap(),
            DiffMatchPatch::patch_to_text(&patches)
        );

        // Character decoding.
        let diffs = vec![
            Diff::delete(b"`1234567890-=[]\\;',./"),
            Diff::insert(b"~!@#$%^&*()_+{}|:\"<>?"),
        ];
        assert_eq!(
            diffs,
            DiffMatchPatch::patch_from_text("@@ -1,21 +1,21 @@\n-%601234567890-=%5B%5D%5C;',./\n+~!@#$%25%5E&*()_+%7B%7D%7C:%22%3C%3E?\n")?[0].diffs
        );

        // Long string with repeats.
        let txt1 = vec!["abcdef"; 100].join("");
        let txt2 = [&txt1, "123"].join("");
        let patches = dmp.patch_make(crate::dmp::PatchInput::Texts(&txt1, &txt2))?;
        assert_eq!(
            "@@ -573,28 +573,31 @@\n cdefabcdefabcdefabcdefabcdef\n+123\n",
            DiffMatchPatch::patch_to_text(&patches)
        );

        Ok(())
    }

    #[test]
    fn test_parse_patch_header() {
        assert_eq!(
            Some((21, Some(4), 21, Some(10))),
            DiffMatchPatch::parse_patch_header("@@ -21,4 +21,10 @@".as_bytes())
        );
        assert_eq!(
            Some((3, None, 3, Some(2))),
            DiffMatchPatch::parse_patch_header("@@ -3 +3,2 @@".as_bytes())
        );

        // Bad cases
        assert!(DiffMatchPatch::parse_patch_header("@@  +3,2 @@".as_bytes()).is_none());
        assert!(DiffMatchPatch::parse_patch_header("@@ 2046 +3,2 @@".as_bytes()).is_none());
    }

    #[test]
    fn test_diff_text() {
        let diffs = vec![
            Diff::equal(b"jump"),
            Diff::delete(b"s"),
            Diff::insert(b"ed"),
            Diff::equal(b" over "),
            Diff::delete(b"the"),
            Diff::insert(b"a"),
            Diff::equal(b" lazy"),
        ];

        assert_eq!(
            b"jumps over the lazy",
            &DiffMatchPatch::diff_text_old(&diffs[..])[..]
        );
        assert_eq!(
            b"jumped over a lazy",
            &DiffMatchPatch::diff_text_new(&diffs[..])[..]
        );
    }

    #[test]
    fn test_patch_add_padding() -> Result<(), crate::errors::Error> {
        let dmp = DiffMatchPatch::default();
        // Both edges full.
        let mut patches = dmp.patch_make(PatchInput::Texts("", "test"))?;
        assert_eq!(
            "@@ -0,0 +1,4 @@\n+test\n",
            DiffMatchPatch::patch_to_text(&patches)
        );
        dmp.patch_add_padding(&mut patches);
        assert_eq!(
            "@@ -1,8 +1,12 @@\n %01%02%03%04\n+test\n %01%02%03%04\n",
            DiffMatchPatch::patch_to_text(&patches)
        );

        // Both edges partial.
        let mut patches = dmp.patch_make(PatchInput::Texts("XY", "XtestY"))?;
        assert_eq!(
            "@@ -1,2 +1,6 @@\n X\n+test\n Y\n",
            DiffMatchPatch::patch_to_text(&patches)
        );
        dmp.patch_add_padding(&mut patches);
        assert_eq!(
            "@@ -2,8 +2,12 @@\n %02%03%04X\n+test\n Y%01%02%03\n",
            DiffMatchPatch::patch_to_text(&patches)
        );

        // Both edges none.
        let mut patches = dmp.patch_make(PatchInput::Texts("XXXXYYYY", "XXXXtestYYYY"))?;
        assert_eq!(
            "@@ -1,8 +1,12 @@\n XXXX\n+test\n YYYY\n",
            DiffMatchPatch::patch_to_text(&patches)
        );
        dmp.patch_add_padding(&mut patches);
        assert_eq!(
            percent_encoding::percent_decode(b"@@ -5,8 +5,12 @@\n XXXX\n+test\n YYYY\n")
                .decode_utf8()
                .unwrap(),
            DiffMatchPatch::patch_to_text(&patches)
        );

        Ok(())
    }

    #[test]
    fn test_patch_split_max() -> Result<(), crate::errors::Error> {
        let dmp = DiffMatchPatch::default();

        // Assumes that dmp.Match_MaxBits is 32.
        let mut patches = dmp.patch_make(PatchInput::Texts(
            "abcdefghijklmnopqrstuvwxyz01234567890",
            "XabXcdXefXghXijXklXmnXopXqrXstXuvXwxXyzX01X23X45X67X89X0",
        ))?;
        dmp.split_max(&mut patches);
        assert_eq!(
            "@@ -1,32 +1,46 @@\n+X\n ab\n+X\n cd\n+X\n ef\n+X\n gh\n+X\n ij\n+X\n kl\n+X\n mn\n+X\n op\n+X\n qr\n+X\n st\n+X\n uv\n+X\n wx\n+X\n yz\n+X\n 012345\n@@ -25,13 +39,18 @@\n zX01\n+X\n 23\n+X\n 45\n+X\n 67\n+X\n 89\n+X\n 0\n",
            DiffMatchPatch::patch_to_text(&patches)
        );

        let mut patches = dmp.patch_make(PatchInput::Texts(
            "abcdef1234567890123456789012345678901234567890123456789012345678901234567890uvwxyz",
            "abcdefuvwxyz",
        ))?;
        let p2t = DiffMatchPatch::patch_to_text(&patches);
        dmp.split_max(&mut patches);
        assert_eq!(p2t, DiffMatchPatch::patch_to_text(&patches));

        let mut patches = dmp.patch_make(PatchInput::Texts(
            "1234567890123456789012345678901234567890123456789012345678901234567890",
            "abc",
        ))?;
        dmp.split_max(&mut patches);
        assert_eq!(
            "@@ -1,32 +1,4 @@\n-1234567890123456789012345678\n 9012\n@@ -29,32 +1,4 @@\n-9012345678901234567890123456\n 7890\n@@ -57,14 +1,3 @@\n-78901234567890\n+abc\n",
            DiffMatchPatch::patch_to_text(&patches)
        );

        let mut patches = dmp.patch_make(PatchInput::Texts(
            "abcdefghij , h : 0 , t : 1 abcdefghij , h : 0 , t : 1 abcdefghij , h : 0 , t : 1",
            "abcdefghij , h : 1 , t : 1 abcdefghij , h : 1 , t : 1 abcdefghij , h : 0 , t : 1",
        ))?;
        dmp.split_max(&mut patches);
        assert_eq!(
            "@@ -2,32 +2,32 @@\n bcdefghij , h : \n-0\n+1\n  , t : 1 abcdef\n@@ -29,32 +29,32 @@\n bcdefghij , h : \n-0\n+1\n  , t : 1 abcdef\n",
            DiffMatchPatch::patch_to_text(&patches)
        );

        Ok(())
    }

    #[test]
    fn test_patch_apply() -> Result<(), crate::errors::Error> {
        let mut dmp = DiffMatchPatch::default();

        let patches = dmp.patch_make(PatchInput::Texts("", ""))?;
        let (txt, results) = dmp.patch_apply_internal(&patches, b"Hello world.")?;
        assert_eq!(
            format!("{}\t{}", std::str::from_utf8(&txt).unwrap(), results.len()),
            "Hello world.\t0"
        );

        let patches = dmp.patch_make(PatchInput::Texts(
            "The quick brown fox jumps over the lazy dog.",
            "That quick brown fox jumped over a lazy dog.",
        ))?;

        // Exact match
        assert_eq!(
            (
                b"That quick brown fox jumped over a lazy dog.".to_vec(),
                vec![true, true]
            ),
            dmp.patch_apply_internal(&patches, b"The quick brown fox jumps over the lazy dog.")?
        );

        // Partial match
        assert_eq!(
            (
                b"That quick red rabbit jumped over a tired tiger.".to_vec(),
                vec![true, true]
            ),
            dmp.patch_apply_internal(
                &patches,
                b"The quick red rabbit jumps over the tired tiger."
            )?
        );

        // Failed match
        assert_eq!(
            (
                b"I am the very model of a modern major general.".to_vec(),
                vec![false, false]
            ),
            dmp.patch_apply_internal(&patches, b"I am the very model of a modern major general.")?
        );

        // Big delete, small change
        let patches = dmp.patch_make(PatchInput::Texts(
            "x1234567890123456789012345678901234567890123456789012345678901234567890y",
            "xabcy",
        ))?;
        assert_eq!((b"xabcy".to_vec(), vec![true, true]), dmp.patch_apply_internal(&patches, b"x123456789012345678901234567890-----++++++++++-----123456789012345678901234567890y")?);

        // Big delete, large change
        let patches = dmp.patch_make(PatchInput::Texts(
            "x1234567890123456789012345678901234567890123456789012345678901234567890y",
            "xabcy",
        ))?;
        assert_eq!((b"xabc12345678901234567890---------------++++++++++---------------12345678901234567890y".to_vec(), vec![false, true]), dmp.patch_apply_internal(&patches, b"x12345678901234567890---------------++++++++++---------------12345678901234567890y")?);

        dmp.delete_threshold = 0.6;
        let patches = dmp.patch_make(PatchInput::Texts(
            "x1234567890123456789012345678901234567890123456789012345678901234567890y",
            "xabcy",
        ))?;
        assert_eq!((b"xabcy".to_vec(), vec![true, true]), dmp.patch_apply_internal(&patches, b"x12345678901234567890---------------++++++++++---------------12345678901234567890y")?);
        dmp.delete_threshold = 0.5;

        // Compesate for failed patch
        dmp.match_threshold = 0.;
        dmp.match_distance = 0;
        let patches = dmp.patch_make(PatchInput::Texts(
            "abcdefghijklmnopqrstuvwxyz--------------------1234567890",
            "abcXXXXXXXXXXdefghijklmnopqrstuvwxyz--------------------1234567YYYYYYYYYY890",
        ))?;
        assert_eq!(
            (
                b"ABCDEFGHIJKLMNOPQRSTUVWXYZ--------------------1234567YYYYYYYYYY890".to_vec(),
                vec![false, true]
            ),
            dmp.patch_apply_internal(
                &patches,
                b"ABCDEFGHIJKLMNOPQRSTUVWXYZ--------------------1234567890"
            )?
        );
        dmp.match_threshold = 0.5;
        dmp.match_distance = 1000;

        // No side-effects - kinds useless cos patches is not mutable in rust
        let patches = dmp.patch_make(PatchInput::Texts("", "test"))?;
        let srcstr = DiffMatchPatch::patch_to_text(&patches);
        dmp.patch_apply_internal(&patches, b"")?;
        assert_eq!(srcstr, DiffMatchPatch::patch_to_text(&patches));

        let patches = dmp.patch_make(PatchInput::Texts(
            "The quick brown fox jumps over the lazy dog.",
            "Woof",
        ))?;
        let srcstr = DiffMatchPatch::patch_to_text(&patches);
        dmp.patch_apply_internal(&patches, b"The quick brown fox jumps over the lazy dog.")?;
        assert_eq!(srcstr, DiffMatchPatch::patch_to_text(&patches));

        // Edge exact match
        let patches = dmp.patch_make(PatchInput::Texts("", "test"))?;
        assert_eq!(
            (b"test".to_vec(), vec![true]),
            dmp.patch_apply_internal(&patches, b"")?
        );

        // Near edge exact match
        let patches = dmp.patch_make(PatchInput::Texts("XY", "XtestY"))?;
        assert_eq!(
            (b"XtestY".to_vec(), vec![true]),
            dmp.patch_apply_internal(&patches, b"XY")?
        );

        // Edge partial match
        let patches = dmp.patch_make(PatchInput::Texts("y", "y123"))?;
        assert_eq!(
            (b"x123".to_vec(), vec![true]),
            dmp.patch_apply_internal(&patches, b"x")?
        );

        Ok(())
    }

    #[test]
    fn test_match_alphabet() {
        // Initialise the bitmasks for Bitap.
        // Unique.
        assert_eq!(
            HashMap::from([(b'a', 4), (b'b', 2), (b'c', 1)]),
            DiffMatchPatch::match_alphabet(b"abc")
        );

        // Duplicates.
        assert_eq!(
            HashMap::from([(b'a', 37), (b'b', 18), (b'c', 8)]),
            DiffMatchPatch::match_alphabet(b"abcaba")
        )
    }

    #[test]
    fn test_match_bitap() {
        // Bitap algorithm.
        let mut dmp = DiffMatchPatch {
            match_distance: 100,
            ..Default::default()
        };

        // Exact matches.
        assert_eq!(Some(5), dmp.match_bitap(b"abcdefghijk", b"fgh", 5));
        assert_eq!(Some(5), dmp.match_bitap(b"abcdefghijk", b"fgh", 0));

        // Fuzzy matches.
        assert_eq!(Some(4), dmp.match_bitap(b"abcdefghijk", b"efxhi", 0));
        assert_eq!(Some(2), dmp.match_bitap(b"abcdefghijk", b"cdefxyhijk", 5));
        assert_eq!(None, dmp.match_bitap(b"abcdefghijk", b"bxy", 1));

        // Overflow.
        assert_eq!(Some(2), dmp.match_bitap(b"123456789xx0", b"3456789x0", 2));

        // Threshold test.
        dmp.match_threshold = 0.4;
        assert_eq!(Some(4), dmp.match_bitap(b"abcdefghijk", b"efxyhi", 1));

        // dmp.Match_Threshold = 0.3;
        dmp.match_threshold = 0.3;
        assert_eq!(None, dmp.match_bitap(b"abcdefghijk", b"efxyhi", 1));

        dmp.match_threshold = 0.;
        assert_eq!(Some(1), dmp.match_bitap(b"abcdefghijk", b"bcdef", 1));

        dmp.match_threshold = 0.5;

        // Multiple select.
        assert_eq!(Some(0), dmp.match_bitap(b"abcdexyzabcde", b"abccde", 3));
        assert_eq!(Some(8), dmp.match_bitap(b"abcdexyzabcde", b"abccde", 5));

        // Distance test.
        dmp.match_distance = 10;
        assert_eq!(
            None,
            dmp.match_bitap(b"abcdefghijklmnopqrstuvwxyz", b"abcdefg", 24)
        );
        assert_eq!(
            Some(0),
            dmp.match_bitap(b"abcdefghijklmnopqrstuvwxyz", b"abcdxxefg", 1)
        );

        dmp.match_distance = 1000;
        assert_eq!(
            Some(0),
            dmp.match_bitap(b"abcdefghijklmnopqrstuvwxyz", b"abcdefg", 24)
        );
    }

    #[test]
    fn test_match_main() {
        let dmp = DiffMatchPatch::default();
        // Full match.
        // Shortcut matches.
        assert_eq!(Some(0), dmp.match_internal(b"abcdef", b"abcdef", 1000));
        assert_eq!(None, dmp.match_internal(b"", b"abcdef", 1));
        assert_eq!(Some(3), dmp.match_internal(b"abcdef", b"", 3));
        assert_eq!(Some(3), dmp.match_internal(b"abcdef", b"de", 3));

        // Beyond end match.
        assert_eq!(Some(3), dmp.match_internal(b"abcdef", b"defy", 4));

        // Oversized pattern.
        assert_eq!(Some(0), dmp.match_internal(b"abcdef", b"abcdefy", 0));

        // Complex match.
        assert_eq!(
            Some(4),
            dmp.match_internal(
                b"I am the very model of a modern major general.",
                b" that berry ",
                5
            )
        );
    }
}

use core::str;
use std::{char, collections::HashMap, fmt::Display};

use chrono::{NaiveTime, TimeDelta, Utc};

use crate::{errors::Error, html::HtmlConfig, DType, PatchInput};

/// Enum representing the different ops of diff
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
pub struct Diff<T: DType>(pub(crate) Ops, pub(crate) Vec<T>);

impl Display for Diff<u8> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({:?}, {})",
            self.op(),
            std::str::from_utf8(self.data()).unwrap()
        )
    }
}

impl Display for Diff<char> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({:?}, {})",
            self.op(),
            self.data().iter().collect::<String>()
        )
    }
}

impl<T: DType> Diff<T> {
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
    // Cost of an empty edit operation in terms of edit characters. Defaults to 4
    edit_cost: usize,
    /// At what point is no match declared (0.0 = perfection, 1.0 = very loose).
    match_threshold: f32,
    /// How far to search for a match (0 = exact location, 1000+ = broad match).
    /// A match this many characters away from the expected location will add
    /// 1.0 to the score (0.0 is a perfect match).
    /// int Match_Distance;
    match_distance: usize,
    /// The number of bits in an int.
    match_max_bits: usize,
    /// When deleting a large block of text (over ~64 characters), how close does
    /// the contents have to match the expected contents. (0.0 = perfection,
    /// 1.0 = very loose).  Note that `match_threshold` controls how closely the
    /// end points of a delete need to match.
    delete_threshold: f32,
    /// Chunk size for context length.
    patch_margin: u8,
}

impl Default for DiffMatchPatch {
    fn default() -> Self {
        Self {
            checklines: true,
            timeout: Some(1000),
            edit_cost: 4,
            match_threshold: 0.5,
            match_distance: 1000,
            match_max_bits: 32,
            patch_margin: 4,
            delete_threshold: 0.5,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct HalfMatch<'a, T: DType> {
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

    /// Enables or disables `line mode` optimization.
    /// When enabled, the diff algorithm tries to find the `lines` that have changes and apply diff on the same
    ///
    /// This optimization makes sense for text with many lines (~100s), defaults to `true`
    pub fn set_checklines(&mut self, checklines: bool) {
        self.checklines = checklines;
    }

    // returns the configured timeout, defaults to `1`, None or `0` would mean infinite timeout
    fn timeout(&self) -> Option<i64> {
        self.timeout.map(|t| t as i64)
    }

    // returns the current edit cost saved
    fn edit_cost(&self) -> usize {
        self.edit_cost
    }

    /// Update edit cost
    pub fn set_edit_cost(&mut self, edit_cost: usize) {
        self.edit_cost = edit_cost;
    }

    /// Set a timeout in number of `milliseconds`. This creates a cutoff for internal `recursive` function calls
    ///
    /// Defaults to `1000ms` (1 second)
    ///
    /// None means `infinite time`
    pub fn set_timeout(&mut self, tout: Option<u32>) {
        self.timeout = tout;
    }

    /// creates a deadline from the given timeout
    pub fn deadline(&self) -> Option<NaiveTime> {
        self.timeout()
            .and_then(|t| Utc::now().checked_add_signed(TimeDelta::milliseconds(t)))
            .map(|t| t.time())
    }

    // returns configured match_threshold
    fn match_threshold(&self) -> f32 {
        self.match_threshold
    }

    /// The `match_threshold` property determines the cut-off value for a valid match.
    /// If `match_threshold` is closer to 0, the requirements for accuracy increase.
    /// If `match_threshold` is closer to 1 then it is more likely that a match will be found.
    /// The `match_threshold` is, the slower `match_main()` may take to compute.
    ///
    /// defaults to 0.5
    pub fn set_match_threshold(&mut self, threshold: f32) {
        self.match_threshold = threshold
    }

    // returns the current patch margin
    fn patch_margin(&self) -> u8 {
        self.patch_margin
    }

    // returns the configured patch delete threshold
    fn delete_threshold(&self) -> f32 {
        self.delete_threshold
    }

    /// When deleting a large block of text (over ~64 characters), how close does
    /// the contents have to match the expected contents. (0.0 = perfection,
    /// 1.0 = very loose).  Note that `match_threshold` controls how closely the
    /// end points of a delete need to match.
    ///
    /// Defaults to `0.5`
    pub fn set_delete_threshold(&mut self, threshold: f32) {
        self.delete_threshold = threshold;
    }

    // returns the configured max_bits
    fn match_max_bits(&self) -> usize {
        self.match_max_bits
    }

    fn match_distance(&self) -> usize {
        self.match_distance
    }

    /// How far to search for a match (0 = exact location, 1000+ = broad match).
    /// A match this many characters away from the expected location will add
    /// 1.0 to the score (0.0 is a perfect match).
    pub fn set_match_distance(&mut self, distance: usize) {
        self.match_distance = distance
    }

    pub(crate) fn diff_internal<'a, T: DType>(
        &self,
        old_bytes: &'a [T],
        new_bytes: &'a [T],
        linemode: bool,
        deadline: Option<NaiveTime>,
    ) -> Result<Vec<Diff<T>>, crate::errors::Error> {
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

    fn compute<'a, T: DType>(
        &self,
        old: &'a [T],
        new: &'a [T],
        linemode: bool,
        deadline: Option<NaiveTime>,
    ) -> Result<Vec<Diff<T>>, crate::errors::Error> {
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

    fn half_match<'a, T: DType>(&self, old: &'a [T], new: &'a [T]) -> Option<HalfMatch<'a, T>> {
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
    fn line_mode<'a, T: DType>(
        &self,
        old: &'a [T],
        new: &'a [T],
        deadline: Option<NaiveTime>,
    ) -> Result<Vec<Diff<T>>, crate::errors::Error> {
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
    pub fn bisect<'a, T: DType>(
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
    fn half_match_i<'a, T: DType>(
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
    fn common_prefix<T: DType>(lhs: &[T], rhs: &[T], reverse: bool) -> usize {
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
    fn common_overlap<T: DType>(lhs: &[T], rhs: &[T]) -> usize {
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
    fn cleanup_semantic<T: DType>(diffs: &mut Vec<Diff<T>>) {
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
    fn cleanup_semantic_lossless<T: DType>(diffs: &mut Vec<Diff<T>>) {
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
    fn cleanup_semantic_score<T: DType>(one: &[T], two: &[T]) -> u8 {
        let (char1, char2) = if let (Some(&char1), Some(&char2)) = (one.last(), two.first()) {
            if let (Some(c1), Some(c2)) = (char1.as_char(), char2.as_char()) {
                (c1, c2)
            } else {
                return 6;
            }
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

        let blankline_1 = linebreak_1 && T::is_linebreak_end(one);
        let blankline_2 = linebreak_2 && T::is_linebreak_start(two);

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
    fn cleanup_merge<T: DType>(diffs: &mut Vec<Diff<T>>) {
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
    #[inline]
    fn cleanup_efficiency<T: DType>(&self, diffs: &mut Vec<Diff<T>>) {
        if diffs.is_empty() {
            return;
        }
        let edit_cost = self.edit_cost();

        let mut changes = false;
        let mut pointer = 0;

        let mut equalities = vec![];

        let mut last_eq = None;

        // Is there an insertion operation before the last equality.
        let mut pre_ins = false;
        // Is there a deletion operation before the last equality.
        let mut pre_del = false;
        // Is there an insertion operation after the last equality.
        let mut post_ins = false;
        // Is there a deletion operation after the last equality.
        let mut post_del = false;

        while pointer < diffs.len() {
            if diffs[pointer].op() == Ops::Equal {
                // Equality found
                if diffs[pointer].size() < edit_cost && (post_ins || post_del) {
                    // Candidate found.
                    equalities.push(pointer);
                    pre_ins = post_ins;
                    pre_del = post_del;

                    last_eq = Some(diffs[pointer].data().to_vec());
                } else {
                    // Not a candidate, and can never become one.
                    equalities = vec![];
                    last_eq = None;
                }

                post_ins = false;
                post_del = false
            } else {
                // Insert or delete
                if diffs[pointer].op() == Ops::Delete {
                    post_del = true;
                } else {
                    post_ins = true;
                }

                // Five types to be split:
                // <ins>A</ins><del>B</del>XY<ins>C</ins><del>D</del>
                // <ins>A</ins>X<ins>C</ins><del>D</del>
                // <ins>A</ins><del>B</del>X<ins>C</ins>
                // <ins>A</del>X<ins>C</ins><del>D</del>
                // <ins>A</ins><del>B</del>X<del>C</del>

                if let Some(le) = &mut last_eq {
                    if (pre_ins && pre_del && post_ins && post_del)
                        || (le.len() < edit_cost / 2
                            && pre_ins as u8 + pre_del as u8 + post_ins as u8 + post_del as u8 == 3)
                    {
                        // Duplicate record.
                        let item = equalities.pop().unwrap();
                        // change the second copy (after the insert in next line) to Insert
                        diffs[item].0 = Ops::Insert;
                        // add an item
                        diffs.insert(item, Diff::delete(le));

                        last_eq = None;

                        if pre_ins && pre_del {
                            // No changes made which could affect previous entry, keep going.
                            post_ins = true;
                            post_del = true;

                            equalities = vec![];
                        } else {
                            equalities.pop();

                            if let Some(&l) = equalities.last() {
                                pointer = l;
                            } else {
                                pointer = 0;
                                post_ins = false;
                                post_del = false;
                                changes = true;
                                continue;
                            };
                            // pointer = equalitiesLength > 0 ?
                            //     equalities[equalitiesLength - 1] : -1;
                            post_ins = false;
                            post_del = false;
                        }
                        changes = true;
                    }
                }
            }
            pointer += 1;
        }

        if changes {
            Self::cleanup_merge(diffs)
        }
    }

    #[inline]
    fn x_index<T: DType>(diffs: &[Diff<T>], loc: usize) -> usize {
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

    #[inline]
    pub fn diff_text_old<T: DType>(diffs: &[Diff<T>]) -> Vec<T> {
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

    #[inline]
    pub fn diff_text_new<T: DType>(diffs: &[Diff<T>]) -> Vec<T> {
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
    fn split_max<T: DType>(&self, patches: &mut Patches<T>) {
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
struct LineToChars<'a, T: DType> {
    chars_old: Vec<usize>,
    chars_new: Vec<usize>,
    lines: Vec<&'a [T]>,
}

impl DiffMatchPatch {
    #[inline]
    fn lines_to_chars<'a, T: DType>(old: &'a [T], new: &'a [T]) -> LineToChars<'a, T> {
        let mut lines: Vec<&'a [T]> = vec![];
        let mut linehash: HashMap<&'a [T], usize> = HashMap::new();

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
    fn lines_to_chars_internal<'a, T: DType>(
        text: &'a [T],
        array: &mut Vec<&'a [T]>,
        hash: &mut HashMap<&'a [T], usize>,
        maxlines: usize,
    ) -> Vec<usize> {
        let take = maxlines - array.len();

        // let mut lines = ;
        let mut charlist = Vec::with_capacity(take);

        let mut broke = false;
        let mut cursor = 0;

        text.split_inclusive(|u| *u == T::from_char('\n'))
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
    fn chars_to_lines<T: DType>(diffs: &[Diff<usize>], lines: &[&[T]]) -> Vec<Diff<T>> {
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
    fn match_internal<T: DType>(&self, text: &[T], pattern: &[T], loc: usize) -> Option<usize> {
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

    #[inline]
    fn match_bitap<T: DType>(&self, text: &[T], pattern: &[T], loc: usize) -> Option<usize> {
        if pattern.len() > self.match_max_bits() {
            return None;
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

    #[inline]
    fn match_alphabet<T: DType>(pattern: &[T]) -> HashMap<T, usize> {
        let mut map = HashMap::with_capacity(pattern.len());

        pattern.iter().enumerate().for_each(|(i, &p)| {
            let v = map.entry(p).or_insert(0_usize);
            *v |= 1 << (pattern.len() - i - 1)
        });

        map
    }

    // Compute and return the score for a match with e errors and x location.
    // Accesses loc and pattern through being a closure.
    #[inline]
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
#[derive(Debug, Clone)]
pub struct Patch<T: DType> {
    diffs: Vec<Diff<T>>,
    start1: usize,
    start2: usize,
    length1: usize,
    length2: usize,
}

impl<T: DType> Default for Patch<T> {
    fn default() -> Self {
        Self {
            diffs: Vec::new(),
            start1: 0,
            start2: 0,
            length1: 0,
            length2: 0,
        }
    }
}

impl<T: DType> Display for Patch<T> {
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
        for diff in self.diffs.iter() {
            let sign = match diff.op() {
                Ops::Insert => '+',
                Ops::Delete => '-',
                Ops::Equal => ' ',
            };

            let enc = T::percent_encode(diff.data());
            let segment = format!(
                "{sign}{}\n",
                T::to_string(&enc).map_err(|_| std::fmt::Error)?
            );

            segments.push(segment)
        }

        write!(f, "{}", segments.join(""))
    }
}

impl<T: DType> Patch<T> {
    pub fn diffs(&self) -> &[Diff<T>] {
        &self.diffs[..]
    }
}

pub type Patches<T> = Vec<Patch<T>>;

impl DiffMatchPatch {
    #[inline]
    fn parse_patch_header<T: DType>(
        s: &[T],
    ) -> Option<(usize, Option<usize>, usize, Option<usize>)> {
        let mut section = Vec::with_capacity(64);
        let mut current_sect = 0;

        let mut old_line = 0;
        let mut old_cols = None;
        let mut new_line = 0;
        let mut new_cols = None;

        for &c in s.iter() {
            if c == T::from_char(' ') {
                match current_sect {
                    0 => {
                        if section != T::from_str("@@") {
                            return None;
                        }
                    }
                    1 => {
                        if section.is_empty() {
                            return None;
                        }

                        let splits = section[1..]
                            .split(|&p| p == T::from_char(','))
                            .collect::<Vec<_>>();

                        let ol = splits.first()?;
                        old_line = T::to_string(ol).ok()?.parse::<usize>().ok()?;
                        if let Some(&oc) = splits.get(1) {
                            old_cols = Some(T::to_string(oc).ok()?.parse::<usize>().ok()?);
                        }
                    }
                    2 => {
                        let splits = section[if *section.first()? == T::from_char('+') {
                            1
                        } else {
                            0
                        }..]
                            .split(|&p| p == T::from_char(','))
                            .collect::<Vec<_>>();

                        let nl = splits.first()?;
                        new_line = T::to_string(nl).ok()?.parse::<usize>().ok()?;
                        if let Some(&nc) = splits.get(1) {
                            new_cols = Some(T::to_string(nc).ok()?.parse::<usize>().ok()?);
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

            if current_sect == 1 && section.is_empty() && c != T::from_char('-') {
                return None;
            }

            section.push(c);
        }

        if section != T::from_str("@@") {
            return None;
        }

        Some((old_line, old_cols, new_line, new_cols))
    }

    #[inline]
    fn patch_make_internal<T: DType>(
        &self,
        txt: &[T],
        diffs: &[Diff<T>],
    ) -> Result<Patches<T>, crate::errors::Error> {
        // No diffs -> no patches
        if diffs.is_empty() {
            return Ok(Vec::new());
        }

        let patch_margin = self.patch_margin() as usize;

        let mut patches = vec![];

        let mut patch = Patch::default();
        let mut char_n1 = 0;
        let mut char_n2 = 0;

        let mut prepatch: Vec<T> = txt.to_vec();
        let mut postpatch: Vec<T> = prepatch.clone();

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

    fn patch_add_context<T: DType>(&self, patch: &mut Patch<T>, text: &[T]) {
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

    fn patch_apply_internal<T: DType>(
        &self,
        patches: &Patches<T>,
        source: &[T],
    ) -> Result<(Vec<T>, Vec<bool>), crate::errors::Error> {
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
                        && (self.diff_levenshtein(&diffs) as f32 / txt_old.len() as f32)
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

    fn patch_add_padding<T: DType>(&self, patches: &mut Patches<T>) -> Vec<T> {
        let null_pad = (1..self.patch_margin() + 1)
            .filter_map(|c| c.as_char().map(|c| T::from_char(c)))
            .collect::<Vec<_>>();

        let pad_len = self.patch_margin() as usize;

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

// Public APIs
impl DiffMatchPatch {
    /// Create a new instance of the struct with default settings
    /// # Example
    /// ```
    /// use diff_match_patch_rs::{DiffMatchPatch, Error, Efficient};
    ///
    /// # fn main() -> Result<(), Error> {
    /// let mut dmp = DiffMatchPatch::new();
    /// // change some settings, e.g. set `line mode` optimization to `false` because you know you have a small text and not many lines
    /// dmp.set_checklines(false);
    /// // do the diffing
    /// let diffs = dmp.diff_main::<Efficient>("Fast enough", "Blazing fast")?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn new() -> Self {
        Self::default()
    }
    /// Find the differences between two texts (old and new).  Simplifies the problem by stripping any common prefix or suffix off the texts before diffing.
    ///
    /// Returns:
    /// Vec of changes (Diff).
    /// # Example
    /// ```
    /// # use diff_match_patch_rs::{DiffMatchPatch, Error, Efficient};
    ///
    /// # fn main() -> Result<(), Error> {
    /// let mut dmp = DiffMatchPatch::new();
    /// // change some settings, e.g. set `line mode` optimization to `false` because you know you have a small text and not many lines
    /// dmp.set_checklines(false);
    /// // do the diffing
    /// let diffs = dmp.diff_main::<Efficient>("Fast enough", "Blazing fast")?;
    /// println!("{}", diffs.iter().map(|d| format!("d")).collect::<Vec<_>>().join("\n"));
    /// // You should see the following output
    /// // (Delete, F)
    /// // (Insert, Blazing f)
    /// // (Equal, ast)
    /// // (Delete,  enough)
    /// # Ok(())
    /// # }
    /// ```
    pub fn diff_main<T: DType>(
        &self,
        old: &str,
        new: &str,
    ) -> Result<Vec<Diff<T>>, crate::errors::Error> {
        let old = T::from_str(old);
        let new = T::from_str(new);

        self.diff_internal(&old, &new, self.checklines(), self.deadline())
    }

    /// A diff of two unrelated texts can be filled with coincidental matches.
    /// For example, the diff of "mouse" and "sofas" is [(-1, "m"), (1, "s"), (0, "o"), (-1, "u"), (1, "fa"), (0, "s"), (-1, "e")].
    /// While this is the optimum diff, it is difficult for humans to understand. Semantic cleanup rewrites the diff, expanding it into a more intelligible format.
    /// The above example would become: [(-1, "mouse"), (1, "sofas")]. If a diff is to be human-readable, it should be passed to diff_cleanup_semantic.
    pub fn diff_cleanup_semantic(diffs: &mut Vec<Diff<u8>>) {
        Self::cleanup_semantic(diffs)
    }

    /// This function is similar to diff_cleanupSemantic, except that instead of optimising a diff to be human-readable, it optimises the diff to be efficient for machine processing.
    /// The results of both cleanup types are often the same.
    ///
    /// The efficiency cleanup is based on the observation that a diff made up of large numbers of small diffs edits may take longer to process (in downstream applications) or take more capacity to store or transmit than a smaller number of larger diffs.
    /// The diff_match_patch.Diff_EditCost property sets what the cost of handling a new edit is in terms of handling extra characters in an existing edit.
    /// The default value is 4, which means if expanding the length of a diff by three characters can eliminate one edit, then that optimisation will reduce the total costs.
    pub fn diff_cleanup_efficiency(&self, diffs: &mut Vec<Diff<u8>>) {
        self.cleanup_efficiency(diffs)
    }

    /// Given a diff, measure its Levenshtein distance in terms of the number of inserted, deleted or substituted characters.
    /// The minimum distance is 0 which means equality, the maximum distance is the length of the longer string.
    pub fn diff_levenshtein<T: DType>(&self, diffs: &[Diff<T>]) -> usize {
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
    ///
    /// # Example
    /// ```
    /// # use diff_match_patch_rs::{DiffMatchPatch, Error, Efficient, HtmlConfig};
    /// # fn main() -> Result<(), Error> {
    /// let dmp = DiffMatchPatch::new();
    ///
    /// let diffs = dmp.diff_main::<Efficient>("The old man and the new house?", "The old man and the old dog!")?;
    /// let htmlcfg = HtmlConfig::new();
    ///
    /// let pretty = dmp.diff_pretty_html(&diffs, &htmlcfg)?;
    /// // Should print: "<span>The old man and the </span><del>new house?</del><ins>old dog!</ins>"
    /// println!("{pretty}");
    ///
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// Check out [`HtmlConfig`] options for ways to control the generated html.
    ///
    /// [`HtmlConfig`]: html/struct.HtmlConfig.html
    pub fn diff_pretty_html<T: DType>(
        &self,
        diffs: &[Diff<T>],
        html_cfg: &HtmlConfig,
    ) -> Result<String, crate::errors::Error> {
        let mut diffs = diffs.to_vec();
        DiffMatchPatch::cleanup_semantic(&mut diffs);

        T::humanize(&mut diffs)?;

        let mut is_err = false;
        let html = diffs
            .iter()
            .filter_map(|diff| {
                let txt = match T::to_string(diff.data()) {
                    Ok(txt) => {
                        let mut txt = txt
                            .replace("&", "&amp;")
                            .replace("<", "&lt;")
                            .replace(">", "&gt;");

                        if html_cfg.nltobr() {
                            txt = txt.replace('\n', "<br>")
                        }

                        txt
                    }
                    Err(e) => {
                        eprintln!("{e:?}");
                        is_err = true;
                        "error".to_string()
                    }
                };

                if txt.is_empty() {
                    return None;
                }

                match diff.op() {
                    Ops::Insert => Some(format!(
                        "<{}{}{}>{txt}</{}>",
                        html_cfg.insert_tag(),
                        if let Some(cl) = html_cfg.insert_class() {
                            format!(" class=\"{cl}\"")
                        } else {
                            String::new()
                        },
                        if let Some(st) = html_cfg.insert_style() {
                            format!(" style=\"{st}\"")
                        } else {
                            String::new()
                        },
                        html_cfg.insert_tag()
                    )),
                    Ops::Delete => Some(format!(
                        "<{}{}{}>{txt}</{}>",
                        html_cfg.delete_tag(),
                        if let Some(cl) = html_cfg.delete_class() {
                            format!(" class=\"{cl}\"")
                        } else {
                            String::new()
                        },
                        if let Some(st) = html_cfg.delete_style() {
                            format!(" style=\"{st}\"")
                        } else {
                            String::new()
                        },
                        html_cfg.delete_tag()
                    )),
                    Ops::Equal => Some(format!(
                        "<{}{}{}>{txt}</{}>",
                        html_cfg.equality_tag(),
                        if let Some(cl) = html_cfg.equality_class() {
                            format!(" class=\"{cl}\"")
                        } else {
                            String::new()
                        },
                        if let Some(st) = html_cfg.equality_style() {
                            format!(" style=\"{st}\"")
                        } else {
                            String::new()
                        },
                        html_cfg.equality_tag()
                    )),
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

    /// Given a text to search, a pattern to search for and an expected location in the text near which to find the pattern, return the location which matches closest.
    /// The function will search for the best match based on both the number of character errors between the pattern and the potential match,
    /// as well as the distance between the expected location and the potential match.
    ///
    /// The following example is a classic dilemma. There are two potential matches, one is close to the expected location but contains a one character error,
    /// the other is far from the expected location but is exactly the pattern sought after: match_main("abc12345678901234567890abbc", "abc", 26)
    /// Which result is returned (0 or 24) is determined by the diff_match_patch.match_distance property.
    /// An exact letter match which is 'distance' characters away from the fuzzy location would score as a complete mismatch.
    /// For example, a distance of '0' requires the match be at the exact location specified, whereas a threshold of '1000' would require a perfect match to be within 800
    /// characters of the expected location to be found using a 0.8 threshold (see below).
    /// The larger match_distance is, the slower match_main() may take to compute. This variable defaults to 1000.
    ///
    /// Another property is `diff_match_patch.match_threshold` which determines the cut-off value for a valid match.
    /// If `match_threshold` is closer to 0, the requirements for accuracy increase.
    /// If `match_threshold` is closer to 1 then it is more likely that a match will be found.
    /// The larger `match_threshold` is, the slower match_main() may take to compute. `match_threshold` defaults to 0.5 and can be updated by `dmp.set_match_threshold()` method.
    ///
    /// If no match is found, the function returns -1.
    pub fn match_main(&self, text: &str, pattern: &str, loc: usize) -> Option<usize> {
        self.match_internal(text.as_bytes(), pattern.as_bytes(), loc)
    }

    /// Given two texts, or an already computed list of differences (`diffs`), return an array of patch objects.
    ///
    /// # Example
    /// ```
    /// # use diff_match_patch_rs::{DiffMatchPatch, Error, Efficient, PatchInput};
    ///
    /// # fn main() -> Result<(), Error> {
    /// let dmp = DiffMatchPatch::new();
    ///
    /// // You can also make patches from the old and new string directly
    /// let patches = dmp.patch_make::<Efficient>(PatchInput::new_text_text("Apples are a fruit.", "Bananas are also fruit"))?;
    /// let (new_from_old, _) = dmp.patch_apply(&patches, "Apples are a fruit.")?;
    /// assert_eq!("Bananas are also fruit", new_from_old);
    ///
    /// // Or, create some diffs in `Efficient` or `Compact` mode
    /// let diffs = dmp.diff_main::<Efficient>("Apples are a fruit.", "Bananas are also fruit")?;
    /// // Now, lets convert the diffs to a bunch of patches - you can use an existing set of diffs to create patches
    /// let patches = dmp.patch_make(PatchInput::new_diffs(&diffs))?;
    /// let (new_from_old, _) = dmp.patch_apply(&patches, "Apples are a fruit.")?;
    /// assert_eq!("Bananas are also fruit", new_from_old);
    ///
    /// // Or, from the source texts and diffs
    /// let patches = dmp.patch_make(PatchInput::new_text_diffs("Apples are a fruit.", &diffs))?;
    /// let (new_from_old, _) = dmp.patch_apply(&patches, "Apples are a fruit.")?;
    /// assert_eq!("Bananas are also fruit", new_from_old);
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// The [`PatchInput::new_text_diffs`] method is preferred, use it if you happen to have that data available, otherwise this function will compute the missing pieces.
    ///
    ///
    pub fn patch_make<T: DType>(
        &self,
        input: PatchInput<T>,
    ) -> Result<Patches<T>, crate::errors::Error> {
        let mut diff_input;
        let txt_old;
        let (txt, diffs) = match input {
            // No diffs provided, lets make our own
            PatchInput::Texts(txt1, txt2) => {
                diff_input = self.diff_main(txt1, txt2)?;
                if diff_input.len() > 2 {
                    Self::cleanup_semantic(&mut diff_input);
                    self.cleanup_efficiency(&mut diff_input);
                }

                (T::from_str(txt1), &diff_input[..])
            }
            PatchInput::Diffs(diffs) => {
                // No origin string provided, compute our own.

                (Self::diff_text_old(diffs), diffs)
            }
            PatchInput::TextDiffs(txt, diffs) => {
                txt_old = T::from_str(txt);
                (txt_old, diffs)
            }
        };

        self.patch_make_internal(&txt, diffs)
    }

    /// Reduces an array of patch objects to a block of text which looks extremely similar to the standard GNU diff/patch format. This text may be stored or transmitted.
    ///
    /// # Example
    ///
    /// ```
    /// # use diff_match_patch_rs::{DiffMatchPatch, Error, Compat, PatchInput};
    ///
    /// # fn main() -> Result<(), Error> {
    /// let dmp = DiffMatchPatch::new();
    ///
    /// // Making patches from source and edited text - both in `Efficient` and `Compat` mode
    /// let patches = dmp.patch_make::<Compat>(PatchInput::new_text_text("Apples are fruit!", "Bananas are also fruit!"))?;
    /// let patch_to_text = dmp.patch_to_text(&patches);
    ///
    /// // Prints patches in GNU diff/ patch format
    /// // You can use this format for transmission and/ or storage.
    /// println!("{patch_to_text}");
    ///
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// Check out the [`diff_to_delta`] and [`diff_from_delta`] methods for a more compact way of representing diffs.
    ///
    /// [`diff_to_delta`]: ./method.diff_to_delta
    /// [`diff_from_delta`]: ./method.diff_from_delta
    ///
    pub fn patch_to_text<T: DType>(&self, patches: &Patches<T>) -> String {
        patches.iter().map(|p| p.to_string()).collect::<String>()
    }

    /// Parses a block of text (which was presumably created by the [`patch_to_text`] method) and returns an array of patch objects.
    ///
    /// # Example
    ///
    /// ```
    /// # use diff_match_patch_rs::{DiffMatchPatch, Error, Compat, PatchInput};
    ///
    /// # fn main() -> Result<(), Error> {
    /// let dmp = DiffMatchPatch::new();
    ///
    /// // Making patches from source and edited text - both in `Efficient` and `Compat` mode
    /// let patches = dmp.patch_make::<Compat>(PatchInput::new_text_text("Apples are fruit!", "Bananas are also fruit!"))?;
    /// let patch_to_text = dmp.patch_to_text(&patches);
    ///
    /// // let's create patches back from text
    /// let patches_recreated = dmp.patch_from_text::<Compat>(&patch_to_text)?;
    ///
    /// // Now you can `patch_apply` the `patches_recreated` to your source text
    ///
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// [`patch_to_text`]: ./method.patch_to_text
    pub fn patch_from_text<T: DType>(&self, text: &str) -> Result<Patches<T>, Error> {
        if text.is_empty() {
            return Ok(vec![]);
        }

        let txt_t = T::from_str(text);
        let mut text = txt_t
            .split(|&p| p == T::from_char('\n'))
            .collect::<Vec<_>>();

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
                let &sign = txt.first().unwrap();

                let line = T::percent_decode(&txt[1..]);

                if sign == T::from_char('-') {
                    patch.diffs.push(Diff::delete(&line));
                } else if sign == T::from_char('+') {
                    patch.diffs.push(Diff::insert(&line));
                } else if sign == T::from_char(' ') {
                    patch.diffs.push(Diff::equal(&line));
                } else if sign == T::from_char('@') {
                    // next patch, break
                    break;
                } else {
                    return Err(Error::InvalidInput);
                }

                text.remove(0);
            }

            patches.push(patch);
        }

        Ok(patches)
    }

    /// Crush the diff into an encoded string which describes the operations required to transform text_old into text_new.
    /// E.g. =3\t-2\t+ing  -> Keep 3 chars, delete 2 chars, insert 'ing'.
    /// Operations are tab-separated. Inserted text is escaped using %xx notation.
    ///
    /// # Example
    ///
    /// ```
    /// # use diff_match_patch_rs::{DiffMatchPatch, Error, Compat, PatchInput};
    /// # fn main() -> Result<(), Error> {
    ///
    /// let dmp = DiffMatchPatch::new();
    ///
    /// // let's create some diffs
    /// let diffs = dmp.diff_main::<Compat>("The old house and the new dog!", "The old man and the new dog!")?;
    /// // now, you can create a `delta` string which can be used to re-create the diffs
    /// let delta = dmp.diff_to_delta(&diffs)?;
    /// println!("{delta:?}");
    /// // You should see something like the following
    /// // "=8\t-5\t+man\t=17"
    ///
    /// // now you can use the `diff_from_delta()` to recover the diffs
    /// let diffs_later = dmp.diff_from_delta::<Compat>("The old house and the new dog!", &delta);
    ///
    /// # Ok(())
    /// # }
    /// ```
    pub fn diff_to_delta<T: DType>(&self, diffs: &[Diff<T>]) -> Result<String, crate::Error> {
        let mut data = diffs
            .iter()
            .map(|diff| match diff.op() {
                Ops::Insert => {
                    let encoded = T::percent_encode(diff.data());
                    [&[T::from_char('+')], &encoded[..], &[T::from_char('\t')]].concat()
                }
                Ops::Delete => [
                    &[T::from_char('-')],
                    &T::from_str(diff.size().to_string().as_str())[..],
                    &[T::from_char('\t')],
                ]
                .concat(),
                Ops::Equal => [
                    &[T::from_char('=')],
                    &T::from_str(diff.size().to_string().as_str())[..],
                    &[T::from_char('\t')],
                ]
                .concat(),
            })
            .collect::<Vec<_>>()
            .concat();

        data.pop();

        T::to_string(&data)
    }

    /// Given the original text `old`, and an encoded string which describes the
    /// operations required to transform text1 into text2, compute the full diff.
    ///
    /// # Example
    ///
    /// ```
    /// # use diff_match_patch_rs::{DiffMatchPatch, Error, Compat, PatchInput};
    /// # fn main() -> Result<(), Error> {
    ///
    /// let dmp = DiffMatchPatch::new();
    ///
    /// // let's create some diffs
    /// let diffs = dmp.diff_main::<Compat>("The old house and the new dog!", "The old man and the new dog!")?;
    /// // now, you can create a `delta` string which can be used to re-create the diffs
    /// let delta = dmp.diff_to_delta(&diffs)?;
    /// println!("{delta:?}");
    /// // You should see something like the following
    /// // "=8\t-5\t+man\t=17"
    ///
    /// // now you can use the `diff_from_delta()` to recover the diffs
    /// let diffs_later = dmp.diff_from_delta::<Compat>("The old house and the new dog!", &delta);
    /// // Now, you can use these diffs to apply patches to the source text
    /// let patches = dmp.patch_make(PatchInput::new_text_diffs("The old house and the new dog!", &diffs))?;
    /// let (new_from_old, _) = dmp.patch_apply(&patches, "The old house and the new dog!")?;
    ///
    ///
    /// assert_eq!("The old man and the new dog!", &new_from_old);
    ///
    /// # Ok(())
    /// # }
    /// ```
    pub fn diff_from_delta<T: DType>(
        &self,
        old: &str,
        delta: &str,
    ) -> Result<Vec<Diff<T>>, crate::errors::Error> {
        let mut pointer = 0; // cursor to text
        let mut diffs = vec![];

        let old = T::from_str(old);
        let delta = T::from_str(delta);

        for token in delta.split(|&k| k == T::from_char('\t')) {
            if token.is_empty() {
                continue;
            }

            // Each token begins with a one character parameter which specifies the
            // operation of this token (delete, insert, equality).
            let opcode = token.first();
            let param = &token[1..];

            if opcode == Some(&T::from_char('+')) {
                let param = T::percent_decode(param);
                diffs.push(Diff::insert(&param));
            } else if opcode == Some(&T::from_char('-')) || opcode == Some(&T::from_char('=')) {
                let n = T::to_string(param)?
                    .parse::<isize>()
                    .map_err(|_| Error::Utf8Error)?;
                if n < 0 {
                    return Err(crate::errors::Error::InvalidInput);
                }

                let n = n as usize;
                let new_pointer = pointer + n;
                if new_pointer > old.len() {
                    return Err(crate::errors::Error::InvalidInput);
                }

                let txt = &old[pointer..new_pointer];
                pointer = new_pointer;

                if opcode == Some(&T::from_char('=')) {
                    diffs.push(Diff::equal(txt))
                } else {
                    diffs.push(Diff::delete(txt))
                }
            } else {
                return Err(crate::errors::Error::InvalidInput);
            }
        }

        if pointer != old.len() {
            return Err(crate::errors::Error::InvalidInput);
        }

        Ok(diffs)
    }

    /// Applies a list of patches to `source_txt`. The first element of the return value is the newly patched text.
    /// The second element is an array of true/false values indicating which of the patches were successfully applied.
    /// [Note that this second element is not too useful since large patches may get broken up internally, resulting in a longer results list than the input with no way to figure out which patch succeeded or failed.
    /// A more informative API is in development.]
    ///
    /// The `match_distance` and `match_threshold` properties are used to evaluate patch application on text which does not match exactly.
    /// In addition, the `DiffMatchPatch.delete_threshold` property determines how closely the text within a major (~64 character) delete needs to match the expected text.
    /// If `delete_threshold` is closer to 0, then the deleted text must match the expected text more closely.
    /// If `delete_threshold` is closer to 1, then the deleted text may contain anything.
    /// In most use cases `delete_threshold` should just be set to the same value as `match_threshold`. Both values default to `0.5`
    ///
    /// # Example
    /// # Example
    /// ```
    /// # use diff_match_patch_rs::{DiffMatchPatch, Error, Efficient, PatchInput};
    ///
    /// # fn main() -> Result<(), Error> {
    /// let dmp = DiffMatchPatch::new();
    ///
    /// // You can also make patches from the old and new string directly
    /// let patches = dmp.patch_make::<Efficient>(PatchInput::new_text_text("Apples are a fruit.", "Bananas are also fruit"))?;
    /// let (new_from_old, _) = dmp.patch_apply(&patches, "Apples are a fruit.")?;
    /// assert_eq!("Bananas are also fruit", new_from_old);
    ///
    /// // Or, create some diffs in `Efficient` or `Compact` mode
    /// let diffs = dmp.diff_main::<Efficient>("Apples are a fruit.", "Bananas are also fruit")?;
    /// // Now, lets convert the diffs to a bunch of patches - you can use an existing set of diffs to create patches
    /// let patches = dmp.patch_make(PatchInput::new_diffs(&diffs))?;
    /// let (new_from_old, _) = dmp.patch_apply(&patches, "Apples are a fruit.")?;
    /// assert_eq!("Bananas are also fruit", new_from_old);
    ///
    /// // Or, from the source texts and diffs
    /// let patches = dmp.patch_make(PatchInput::new_text_diffs("Apples are a fruit.", &diffs))?;
    /// let (new_from_old, _) = dmp.patch_apply(&patches, "Apples are a fruit.")?;
    /// assert_eq!("Bananas are also fruit", new_from_old);
    /// # Ok(())
    /// # }
    /// ```
    pub fn patch_apply<T: DType>(
        &self,
        patches: &Patches<T>,
        source_txt: &str,
    ) -> Result<(String, Vec<bool>), crate::errors::Error> {
        let (str_data, results) = self.patch_apply_internal(patches, &T::from_str(source_txt))?;

        Ok((
            T::to_string(&str_data).map_err(|_| crate::errors::Error::Utf8Error)?,
            results,
        ))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        dmp::{Diff, HalfMatch, LineToChars},
        Compat, DiffMatchPatch, Efficient, Error, Patch, PatchInput,
    };

    #[test]
    fn test_prefix() {
        // Detect any common prefix.
        // Null case.
        assert_eq!(
            0,
            DiffMatchPatch::common_prefix("abc".as_bytes(), "xyz".as_bytes(), false)
        );
        assert_eq!(
            0,
            DiffMatchPatch::common_prefix(
                &"abc".chars().collect::<Vec<_>>()[..],
                &"xyz".chars().collect::<Vec<_>>()[..],
                false
            )
        );

        // Non-null case.
        assert_eq!(
            4,
            DiffMatchPatch::common_prefix("1234abcdef".as_bytes(), "1234xyz".as_bytes(), false)
        );
        assert_eq!(
            4,
            DiffMatchPatch::common_prefix(
                &"1234abcdef".chars().collect::<Vec<_>>()[..],
                &"1234xyz".chars().collect::<Vec<_>>()[..],
                false
            )
        );

        // Whole case.
        assert_eq!(
            4,
            DiffMatchPatch::common_prefix("1234".as_bytes(), "1234xyz".as_bytes(), false)
        );
        assert_eq!(
            4,
            DiffMatchPatch::common_prefix(
                &"1234".chars().collect::<Vec<_>>()[..],
                &"1234xyz".chars().collect::<Vec<_>>()[..],
                false
            )
        );

        // Unicode - difference between `u8`(Efficient) & `char`(Compat)
        // first 3 bytes same
        assert_eq!(
            3,
            DiffMatchPatch::common_prefix("🤪".as_bytes(), "🤔".as_bytes(), false)
        );
        assert_eq!(
            0,
            DiffMatchPatch::common_prefix(
                &"🤪".chars().collect::<Vec<_>>()[..],
                &"🤔".chars().collect::<Vec<_>>()[..],
                false
            )
        );
        // first 2 bytes same
        assert_eq!(
            2,
            DiffMatchPatch::common_prefix("🍊".as_bytes(), "🌊".as_bytes(), false)
        );
        assert_eq!(
            0,
            DiffMatchPatch::common_prefix(
                &"🍊".chars().collect::<Vec<_>>()[..],
                &"🌊".chars().collect::<Vec<_>>()[..],
                false
            )
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
        assert_eq!(
            0,
            DiffMatchPatch::common_prefix(
                &"abc".chars().collect::<Vec<_>>()[..],
                &"xyz".chars().collect::<Vec<_>>()[..],
                true
            )
        );

        // Non-null case.
        assert_eq!(
            4,
            DiffMatchPatch::common_prefix("abcdef1234".as_bytes(), "xyz1234".as_bytes(), true)
        );
        assert_eq!(
            4,
            DiffMatchPatch::common_prefix(
                &"abcdef1234".chars().collect::<Vec<_>>()[..],
                &"xyz1234".chars().collect::<Vec<_>>()[..],
                true
            )
        );

        // Whole case.
        assert_eq!(
            4,
            DiffMatchPatch::common_prefix("1234".as_bytes(), "xyz1234".as_bytes(), true)
        );
        assert_eq!(
            4,
            DiffMatchPatch::common_prefix(
                &"1234".chars().collect::<Vec<_>>()[..],
                &"xyz1234".chars().collect::<Vec<_>>()[..],
                true
            )
        );

        // Unicode - difference between `u8`(Efficient) & `char`(Compat)
        // No common suffix
        assert_eq!(
            0,
            DiffMatchPatch::common_prefix("🍌".as_bytes(), "🌍".as_bytes(), true)
        );
        assert_eq!(
            0,
            DiffMatchPatch::common_prefix(
                &"🍌".chars().collect::<Vec<_>>()[..],
                &"🌍".chars().collect::<Vec<_>>()[..],
                true
            )
        );
        // Last bytes same
        assert_eq!(
            1,
            DiffMatchPatch::common_prefix("🍊".as_bytes(), "🌊".as_bytes(), true)
        );
        assert_eq!(
            0,
            DiffMatchPatch::common_prefix(
                &"🍊".chars().collect::<Vec<_>>()[..],
                &"🌊".chars().collect::<Vec<_>>()[..],
                true
            )
        );
        assert_eq!(
            6,
            DiffMatchPatch::common_prefix("🍊 nah!".as_bytes(), "🌊 nah!".as_bytes(), true)
        );
        assert_eq!(
            5,
            DiffMatchPatch::common_prefix(
                &"🍊 nah!".chars().collect::<Vec<_>>()[..],
                &"🌊 nah!".chars().collect::<Vec<_>>()[..],
                true
            )
        );
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
                chars_old: vec![0_usize, 1, 0],
                chars_new: vec![1_usize, 0, 1],
                lines: vec![
                    &"alpha\n".chars().collect::<Vec<_>>()[..],
                    &"beta\n".chars().collect::<Vec<_>>()[..]
                ]
            },
            DiffMatchPatch::lines_to_chars(
                &"alpha\nbeta\nalpha\n".chars().collect::<Vec<_>>()[..],
                &"beta\nalpha\nbeta\n".chars().collect::<Vec<_>>()[..]
            )
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
                chars_old: vec![],
                chars_new: vec![0_usize, 1, 2, 2],
                lines: vec![
                    &"alpha\r\n".chars().collect::<Vec<_>>()[..],
                    &"beta\r\n".chars().collect::<Vec<_>>()[..],
                    &"\r\n".chars().collect::<Vec<_>>()[..]
                ]
            },
            DiffMatchPatch::lines_to_chars(
                &[],
                &"alpha\r\nbeta\r\n\r\n\r\n".chars().collect::<Vec<_>>()[..]
            )
        );
        assert_eq!(
            LineToChars {
                chars_old: vec![0_usize],
                chars_new: vec![1_usize],
                lines: vec![b"a", b"b"]
            },
            DiffMatchPatch::lines_to_chars(b"a", b"b")
        );
        assert_eq!(
            LineToChars {
                chars_old: vec![0_usize],
                chars_new: vec![1_usize],
                lines: vec![&['a'], &['b']]
            },
            DiffMatchPatch::lines_to_chars(&['a'], &['b'])
        );

        // More than 256 to reveal any 8-bit limitations.
        const TLIMIT: usize = 300;
        let linestr = (0..TLIMIT).map(|i| format!("{i}\n")).collect::<Vec<_>>();
        let linechars = (0..TLIMIT)
            .map(|i| format!("{i}\n").chars().collect::<Vec<_>>())
            .collect::<Vec<_>>();
        let linelist_u8: Vec<&[u8]> = (0..TLIMIT).map(|i| linestr[i].as_bytes()).collect();
        let charlist = (0..TLIMIT).collect::<Vec<_>>();
        let linestring = linestr.join("");
        let res = DiffMatchPatch::lines_to_chars(linestring.as_bytes(), b"");
        let src = LineToChars {
            chars_old: charlist.clone(),
            chars_new: vec![],
            lines: linelist_u8,
        };
        assert_eq!(res.chars_new, src.chars_new);
        assert_eq!(res.lines, src.lines);
        assert_eq!(res.chars_old, src.chars_old);

        let mut linelist_ch: Vec<&[char]> = vec![];
        for lc in linechars.iter() {
            linelist_ch.push(&lc[..]);
        }

        let linestringchars = linestring.chars().collect::<Vec<_>>();
        let res = DiffMatchPatch::lines_to_chars(&linestringchars[..], &[]);
        let src = LineToChars {
            chars_old: charlist,
            chars_new: vec![],
            lines: linelist_ch,
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

        let diffs = [Diff::equal(&d1), Diff::insert(&d2)];
        let diffs = DiffMatchPatch::chars_to_lines(
            &diffs,
            &[
                &"alpha\n".chars().collect::<Vec<_>>()[..],
                &"beta\n".chars().collect::<Vec<_>>()[..],
            ],
        );

        assert_eq!(
            vec![
                Diff::equal(&"alpha\nbeta\nalpha\n".chars().collect::<Vec<_>>()[..]),
                Diff::insert(&"beta\nalpha\nbeta\n".chars().collect::<Vec<_>>()[..])
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

        let charlist = (0..TLIMIT).collect::<Vec<_>>();
        let linestr = (0..TLIMIT).map(|i| format!("{i}\n")).collect::<Vec<_>>();
        let linelist: Vec<Vec<char>> = (0..TLIMIT)
            .map(|i| linestr[i].chars().collect::<Vec<_>>())
            .collect();

        let diffs = [Diff::delete(&charlist)];
        let mut linelistchars = vec![];
        for ll in linelist.iter() {
            linelistchars.push(&ll[..]);
        }
        let diffs = DiffMatchPatch::chars_to_lines(&diffs, &linelistchars);

        assert_eq!(
            vec![Diff::delete(
                &linestr.join("").chars().collect::<Vec<_>>()[..]
            )],
            diffs
        );

        // More than 65536 to verify any 16-bit limitation.
        const ULIMIT: usize = 10;
        let linestr = (0..ULIMIT).map(|i| format!("{i}\n")).collect::<Vec<_>>();
        let lines = linestr.join("");
        let l2c = DiffMatchPatch::lines_to_chars(lines.as_bytes(), b"");

        let diffs = [Diff::insert(&l2c.chars_old)];
        let diffs = DiffMatchPatch::chars_to_lines(&diffs, &l2c.lines);

        assert_eq!(lines.as_bytes(), diffs[0].data());

        let lchars = lines.chars().collect::<Vec<_>>();
        let l2c = DiffMatchPatch::lines_to_chars(&lchars[..], &[]);

        let diffs = [Diff::insert(&l2c.chars_old)];
        let diffs = DiffMatchPatch::chars_to_lines(&diffs, &l2c.lines);

        assert_eq!(&lines.chars().collect::<Vec<_>>()[..], diffs[0].data());
    }

    #[test]
    fn test_diff_cleanup_merge() {
        // Cleanup a messy diff.
        // Null case.
        let mut diffs = vec![];
        let test: Vec<Diff<Efficient>> = vec![];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![];
        let test: Vec<Diff<Compat>> = vec![];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // No change case
        let mut diffs = vec![Diff::equal(b"a"), Diff::delete(b"b"), Diff::insert(b"c")];
        let test = vec![Diff::equal(b"a"), Diff::delete(b"b"), Diff::insert(b"c")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::equal(&['a']),
            Diff::delete(&['b']),
            Diff::insert(&['c']),
        ];
        let test = vec![
            Diff::equal(&['a']),
            Diff::delete(&['b']),
            Diff::insert(&['c']),
        ];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Merge equalities.
        let mut diffs = vec![Diff::equal(b"a"), Diff::equal(b"b"), Diff::equal(b"c")];
        let test = vec![Diff::equal(b"abc")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::equal(&['a']),
            Diff::equal(&['b']),
            Diff::equal(&['c']),
        ];
        let test = vec![Diff::equal(&['a', 'b', 'c'])];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Merge deletions.
        let mut diffs = vec![Diff::delete(b"a"), Diff::delete(b"b"), Diff::delete(b"c")];
        let test = vec![Diff::delete(b"abc")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::delete(&['a']),
            Diff::delete(&['b']),
            Diff::delete(&['c']),
        ];
        let test = vec![Diff::delete(&['a', 'b', 'c'])];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Merge insertions.
        let mut diffs = vec![Diff::insert(b"a"), Diff::insert(b"b"), Diff::insert(b"c")];
        let test = vec![Diff::insert(b"abc")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::insert(&['a']),
            Diff::insert(&['b']),
            Diff::insert(&['c']),
        ];
        let test = vec![Diff::insert(&['a', 'b', 'c'])];
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

        let mut diffs = vec![
            Diff::delete(&['a']),
            Diff::insert(&['b']),
            Diff::delete(&['c']),
            Diff::insert(&['d']),
            Diff::equal(&['e']),
            Diff::equal(&['f']),
        ];
        let test = vec![
            Diff::delete(&['a', 'c']),
            Diff::insert(&['b', 'd']),
            Diff::equal(&['e', 'f']),
        ];
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

        let mut diffs = vec![
            Diff::delete(&['a']),
            Diff::insert(&['a', 'b', 'c']),
            Diff::delete(&['d', 'c']),
        ];
        let test = vec![
            Diff::equal(&['a']),
            Diff::delete(&['d']),
            Diff::insert(&['b']),
            Diff::equal(&['c']),
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
        let mut diffs = vec![
            Diff::equal(&['x']),
            Diff::delete(&['a']),
            Diff::insert(&['a', 'b', 'c']),
            Diff::delete(&['d', 'c']),
            Diff::equal(&['y']),
        ];
        let test = vec![
            Diff::equal(&['x', 'a']),
            Diff::delete(&['d']),
            Diff::insert(&['b']),
            Diff::equal(&['c', 'y']),
        ];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Slide edit left.
        let mut diffs = vec![Diff::equal(b"a"), Diff::insert(b"ba"), Diff::equal(b"c")];
        let test = vec![Diff::insert(b"ab"), Diff::equal(b"ac")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::equal(&['a']),
            Diff::insert(&['b', 'a']),
            Diff::equal(&['c']),
        ];
        let test = vec![Diff::insert(&['a', 'b']), Diff::equal(&['a', 'c'])];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Slide edit right
        let mut diffs = vec![Diff::equal(b"c"), Diff::insert(b"ab"), Diff::equal(b"a")];
        let test = vec![Diff::equal(b"ca"), Diff::insert(b"ba")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::equal(&['c']),
            Diff::insert(&['a', 'b']),
            Diff::equal(&['a']),
        ];
        let test = vec![Diff::equal(&['c', 'a']), Diff::insert(&['b', 'a'])];
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
        let mut diffs = vec![
            Diff::equal(&['a']),
            Diff::delete(&['b']),
            Diff::equal(&['c']),
            Diff::delete(&['a', 'c']),
            Diff::equal(&['x']),
        ];
        let test = vec![
            Diff::delete(&['a', 'b', 'c']),
            Diff::equal(&['a', 'c', 'x']),
        ];
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

        let mut diffs = vec![
            Diff::equal(&['x']),
            Diff::delete(&['c', 'a']),
            Diff::equal(&['c']),
            Diff::delete(&['b']),
            Diff::equal(&['a']),
        ];
        let test = vec![
            Diff::equal(&['x', 'c', 'a']),
            Diff::delete(&['c', 'b', 'a']),
        ];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Empty merge.
        let mut diffs = vec![Diff::delete(b"b"), Diff::insert(b"ab"), Diff::equal(b"c")];
        let test = vec![Diff::insert(b"a"), Diff::equal(b"bc")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::delete(&['b']),
            Diff::insert(&['a', 'b']),
            Diff::equal(&['c']),
        ];
        let test = vec![Diff::insert(&['a']), Diff::equal(&['b', 'c'])];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        // Empty equality.
        let mut diffs = vec![Diff::equal(b""), Diff::insert(b"a"), Diff::equal(b"b")];
        let test = vec![Diff::insert(b"a"), Diff::equal(b"b")];
        DiffMatchPatch::cleanup_merge(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![Diff::equal(&[]), Diff::insert(&['a']), Diff::equal(&['b'])];
        let test = vec![Diff::insert(&['a']), Diff::equal(&['b'])];
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

        let mut diffs = vec![
            Diff::delete(&['a', 'b']),
            Diff::insert(&['c', 'd']),
            Diff::equal(&['1', '2']),
            Diff::delete(&['e']),
        ];
        let test = vec![
            Diff::delete(&['a', 'b']),
            Diff::insert(&['c', 'd']),
            Diff::equal(&['1', '2']),
            Diff::delete(&['e']),
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

        let mut diffs = vec![
            Diff::delete(&['a', 'b', 'c']),
            Diff::insert(&['A', 'B', 'C']),
            Diff::equal(&['1', '2', '3', '4']),
            Diff::delete(&['w', 'x', 'y', 'z']),
        ];
        let test = vec![
            Diff::delete(&['a', 'b', 'c']),
            Diff::insert(&['A', 'B', 'C']),
            Diff::equal(&['1', '2', '3', '4']),
            Diff::delete(&['w', 'x', 'y', 'z']),
        ];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // Simple elimination.
        let mut diffs = vec![Diff::delete(b"a"), Diff::equal(b"b"), Diff::delete(b"c")];
        let test: Vec<Diff<u8>> = vec![Diff::delete(b"abc"), Diff::insert(b"b")];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::delete(&['a']),
            Diff::equal(&['b']),
            Diff::delete(&['c']),
        ];
        let test = vec![Diff::delete(&['a', 'b', 'c']), Diff::insert(&['b'])];
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

        let mut diffs = vec![
            Diff::delete(&['a', 'b']),
            Diff::equal(&['c', 'd']),
            Diff::delete(&['e']),
            Diff::equal(&['f']),
            Diff::insert(&['g']),
        ];
        let test = vec![
            Diff::delete(&['a', 'b', 'c', 'd', 'e', 'f']),
            Diff::insert(&['c', 'd', 'f', 'g']),
        ];
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

        let mut diffs = vec![
            Diff::insert(&['1']),
            Diff::equal(&['A']),
            Diff::delete(&['B']),
            Diff::insert(&['2']),
            Diff::equal(&['_']),
            Diff::insert(&['1']),
            Diff::equal(&['A']),
            Diff::delete(&['B']),
            Diff::insert(&['2']),
        ];
        let test = vec![
            Diff::delete(&['A', 'B', '_', 'A', 'B']),
            Diff::insert(&['1', 'A', '2', '_', '1', 'A', '2']),
        ];
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

        let mut diffs = vec![
            Diff::equal(&"The c".chars().collect::<Vec<_>>()[..]),
            Diff::delete(&"ow and the c".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"at.".chars().collect::<Vec<_>>()[..]),
        ];
        let test = vec![
            Diff::equal(&"The ".chars().collect::<Vec<_>>()[..]),
            Diff::delete(&"cow and the ".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"cat.".chars().collect::<Vec<_>>()[..]),
        ];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // No overlap elimination.
        let mut diffs = vec![Diff::delete(b"abcxx"), Diff::insert(b"xxdef")];
        let test = vec![Diff::delete(b"abcxx"), Diff::insert(b"xxdef")];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::delete(&"abcxx".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"xxdef".chars().collect::<Vec<_>>()[..]),
        ];
        let test = vec![
            Diff::delete(&"abcxx".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"xxdef".chars().collect::<Vec<_>>()[..]),
        ];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // Overlap elimination.
        let mut diffs = vec![Diff::delete(b"abcxxx"), Diff::insert(b"xxxdef")];
        let test = vec![
            Diff::delete(b"abc"),
            Diff::equal(b"xxx"),
            Diff::insert(b"def"),
        ];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::delete(&"abcxxx".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"xxxdef".chars().collect::<Vec<_>>()[..]),
        ];
        let test = vec![
            Diff::delete(&"abc".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"xxx".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"def".chars().collect::<Vec<_>>()[..]),
        ];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // Reverse overlap elimination.
        let mut diffs = vec![
            Diff::delete(&"xxxabc".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"defxxx".chars().collect::<Vec<_>>()[..]),
        ];
        let test = vec![
            Diff::insert(&"def".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"xxx".chars().collect::<Vec<_>>()[..]),
            Diff::delete(&"abc".chars().collect::<Vec<_>>()[..]),
        ];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![Diff::delete(b"xxxabc"), Diff::insert(b"defxxx")];
        let test = vec![
            Diff::insert(b"def"),
            Diff::equal(b"xxx"),
            Diff::delete(b"abc"),
        ];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::delete(&"xxxabc".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"defxxx".chars().collect::<Vec<_>>()[..]),
        ];
        let test = vec![
            Diff::insert(&"def".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"xxx".chars().collect::<Vec<_>>()[..]),
            Diff::delete(&"abc".chars().collect::<Vec<_>>()[..]),
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
        let test = vec![
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

        let mut diffs = vec![
            Diff::delete(&"abcd1212".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"1212efghi".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"----".chars().collect::<Vec<_>>()[..]),
            Diff::delete(&"A3".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"3BC".chars().collect::<Vec<_>>()[..]),
        ];
        let test = vec![
            Diff::delete(&"abcd".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"1212".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"efghi".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"----".chars().collect::<Vec<_>>()[..]),
            Diff::delete(&['A']),
            Diff::equal(&['3']),
            Diff::insert(&['B', 'C']),
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

        let mut diffs: Vec<Diff<char>> = vec![];
        let test: Vec<Diff<char>> = vec![];
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

        let mut diffs = vec![
            Diff::equal(&"AAA\r\n\r\nBBB".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"\r\nDDD\r\n\r\nBBB".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"\r\nEEE".chars().collect::<Vec<_>>()[..]),
        ];
        let test = vec![
            Diff::equal(&"AAA\r\n\r\n".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"BBB\r\nDDD\r\n\r\n".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"BBB\r\nEEE".chars().collect::<Vec<_>>()[..]),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Line boundaries.
        let mut diffs = vec![
            Diff::equal(&"AAA\r\nBBB".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&" DDD\r\nBBB".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&" EEE".chars().collect::<Vec<_>>()[..]),
        ];
        let test = vec![
            Diff::equal(&"AAA\r\n".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"BBB DDD\r\n".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"BBB EEE".chars().collect::<Vec<_>>()[..]),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::equal(b"AAA\r\nBBB"),
            Diff::insert(b" DDD\r\nBBB"),
            Diff::equal(b" EEE"),
        ];
        let test = vec![
            Diff::equal(b"AAA\r\n"),
            Diff::insert(b"BBB DDD\r\n"),
            Diff::equal(b"BBB EEE"),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::equal(&"AAA\r\nBBB".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&" DDD\r\nBBB".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&" EEE".chars().collect::<Vec<_>>()[..]),
        ];
        let test = vec![
            Diff::equal(&"AAA\r\n".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"BBB DDD\r\n".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"BBB EEE".chars().collect::<Vec<_>>()[..]),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Word boundaries.
        let mut diffs = vec![
            Diff::equal(b"The c"),
            Diff::insert(b"ow and the c"),
            Diff::equal(b"at."),
        ];
        let test = vec![
            Diff::equal(b"The "),
            Diff::insert(b"cow and the "),
            Diff::equal(b"cat."),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::equal(&"The c".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"ow and the c".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"at.".chars().collect::<Vec<_>>()[..]),
        ];
        let test = vec![
            Diff::equal(&"The ".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"cow and the ".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"cat.".chars().collect::<Vec<_>>()[..]),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Alphanumeric boundaries.
        let mut diffs = vec![
            Diff::equal(b"The-c"),
            Diff::insert(b"ow-and-the-c"),
            Diff::equal(b"at."),
        ];
        let test = vec![
            Diff::equal(b"The-"),
            Diff::insert(b"cow-and-the-"),
            Diff::equal(b"cat."),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::equal(&"The-c".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"ow-and-the-c".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"at.".chars().collect::<Vec<_>>()[..]),
        ];
        let test = vec![
            Diff::equal(&"The-".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"cow-and-the-".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"cat.".chars().collect::<Vec<_>>()[..]),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Hitting the start.
        let mut diffs = vec![Diff::equal(b"a"), Diff::delete(b"a"), Diff::equal(b"ax")];
        let test = vec![Diff::delete(b"a"), Diff::equal(b"aax")];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::equal(&['a']),
            Diff::delete(&['a']),
            Diff::equal(&['a', 'x']),
        ];
        let test = vec![Diff::delete(&['a']), Diff::equal(&['a', 'a', 'x'])];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Hitting the end.
        let mut diffs = vec![Diff::equal(b"xa"), Diff::delete(b"a"), Diff::equal(b"a")];
        let test = vec![Diff::equal(b"xaa"), Diff::delete(b"a")];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::equal(&['x', 'a']),
            Diff::delete(&['a']),
            Diff::equal(&['a']),
        ];
        let test = vec![Diff::equal(&['x', 'a', 'a']), Diff::delete(&['a'])];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Sentence boundaries.
        let mut diffs = vec![
            Diff::equal(b"The xxx. The "),
            Diff::insert(b"zzz. The "),
            Diff::equal(b"yyy."),
        ];
        let test = vec![
            Diff::equal(b"The xxx."),
            Diff::insert(b" The zzz."),
            Diff::equal(b" The yyy."),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        let mut diffs = vec![
            Diff::equal(&"The xxx. The ".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&"zzz. The ".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&"yyy.".chars().collect::<Vec<_>>()[..]),
        ];
        let test = vec![
            Diff::equal(&"The xxx.".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&" The zzz.".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&" The yyy.".chars().collect::<Vec<_>>()[..]),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);
    }

    #[test]
    fn test_diff_cleanup_effitiency() {
        // testDiffCleanupEfficiency
        // Cleanup operationally trivial equalities.
        let mut dmp = DiffMatchPatch {
            edit_cost: 4,
            ..Default::default()
        };

        // Nullcase
        let mut diffs = vec![];
        dmp.cleanup_efficiency::<Efficient>(&mut diffs);
        assert!(diffs.is_empty());

        let mut diffs = vec![];
        dmp.cleanup_efficiency::<Compat>(&mut diffs);
        assert!(diffs.is_empty());

        // No eliminations
        let mut diffs = vec![
            Diff::delete(b"ab"),
            Diff::insert(b"12"),
            Diff::equal(b"wxyz"),
            Diff::delete(b"cd"),
            Diff::insert(b"34"),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![
                Diff::delete(b"ab"),
                Diff::insert(b"12"),
                Diff::equal(b"wxyz"),
                Diff::delete(b"cd"),
                Diff::insert(b"34")
            ],
            diffs
        );

        let mut diffs = vec![
            Diff::delete(&['a', 'b']),
            Diff::insert(&['1', '2']),
            Diff::equal(&['w', 'x', 'y', 'z']),
            Diff::delete(&['c', 'd']),
            Diff::insert(&['3', '4']),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![
                Diff::delete(&['a', 'b']),
                Diff::insert(&['1', '2']),
                Diff::equal(&['w', 'x', 'y', 'z']),
                Diff::delete(&['c', 'd']),
                Diff::insert(&['3', '4'])
            ],
            diffs
        );

        let mut diffs = vec![
            Diff::delete("😀".as_bytes()),
            Diff::insert(b"12"),
            Diff::equal(b"wxyz"),
            Diff::delete("❤️‍🔥".as_bytes()),
            Diff::insert(b"34"),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![
                Diff::delete("😀".as_bytes()),
                Diff::insert(b"12"),
                Diff::equal(b"wxyz"),
                Diff::delete("❤️‍🔥".as_bytes()),
                Diff::insert(b"34")
            ],
            diffs
        );

        let mut diffs = vec![
            Diff::delete(&['😀']),
            Diff::insert(&['1', '2']),
            Diff::equal(&['w', 'x', 'y', 'z']),
            Diff::delete(&"❤️‍🔥".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&['3', '4']),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![
                Diff::delete(&['😀']),
                Diff::insert(&['1', '2']),
                Diff::equal(&['w', 'x', 'y', 'z']),
                Diff::delete(&"❤️‍🔥".chars().collect::<Vec<_>>()[..]),
                Diff::insert(&['3', '4'])
            ],
            diffs
        );

        // Four edit eliminations
        let mut diffs = vec![
            Diff::delete("😀".as_bytes()),
            Diff::insert(b"12"),
            Diff::equal(b"xyz"),
            Diff::delete("❤️‍🔥".as_bytes()),
            Diff::insert(b"34"),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![
                Diff::delete("😀xyz❤️‍🔥".as_bytes()),
                Diff::insert(b"12xyz34"),
            ],
            diffs
        );

        let mut diffs = vec![
            Diff::delete(&['😀']),
            Diff::insert(&['1', '2']),
            Diff::equal(&['x', 'y', 'z']),
            Diff::delete(&"❤️‍🔥".chars().collect::<Vec<_>>()),
            Diff::insert(&['3', '4']),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![
                Diff::delete(&"😀xyz❤️‍🔥".chars().collect::<Vec<_>>()),
                Diff::insert(&['1', '2', 'x', 'y', 'z', '3', '4']),
            ],
            diffs
        );

        let mut diffs = vec![
            Diff::delete("ab".as_bytes()),
            Diff::insert(b"12"),
            Diff::equal(b"xyz"),
            Diff::delete("cd".as_bytes()),
            Diff::insert(b"34"),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![Diff::delete("abxyzcd".as_bytes()), Diff::insert(b"12xyz34"),],
            diffs
        );

        let mut diffs = vec![
            Diff::delete(&['a', 'b']),
            Diff::insert(&['1', '2']),
            Diff::equal(&['x', 'y', 'z']),
            Diff::delete(&['c', 'd']),
            Diff::insert(&['3', '4']),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![
                Diff::delete(&"abxyzcd".chars().collect::<Vec<_>>()),
                Diff::insert(&['1', '2', 'x', 'y', 'z', '3', '4']),
            ],
            diffs
        );

        // 3 edit eliminations
        let mut diffs = vec![
            Diff::insert("😀".as_bytes()),
            Diff::equal(b"x"),
            Diff::delete("❤️‍🔥".as_bytes()),
            Diff::insert(b"34"),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![
                Diff::delete("x❤️‍🔥".as_bytes()),
                Diff::insert("😀x34".as_bytes()),
            ],
            diffs
        );

        let mut diffs = vec![
            Diff::insert(&['😀']),
            Diff::equal(&['x']),
            Diff::delete(&"❤️‍🔥".chars().collect::<Vec<_>>()),
            Diff::insert(&['3', '4']),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![
                Diff::delete(&"x❤️‍🔥".chars().collect::<Vec<_>>()),
                Diff::insert(&['😀', 'x', '3', '4']),
            ],
            diffs
        );

        let mut diffs = vec![
            Diff::insert(b"12"),
            Diff::equal(b"x"),
            Diff::delete("cd".as_bytes()),
            Diff::insert(b"34"),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![Diff::delete("xcd".as_bytes()), Diff::insert(b"12x34"),],
            diffs
        );

        let mut diffs = vec![
            Diff::insert(&['1', '2']),
            Diff::equal(&['x']),
            Diff::delete(&['c', 'd']),
            Diff::insert(&['3', '4']),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![
                Diff::delete(&"xcd".chars().collect::<Vec<_>>()),
                Diff::insert(&['1', '2', 'x', '3', '4']),
            ],
            diffs
        );

        // backpass eliminations
        let mut diffs = vec![
            Diff::delete(b"ab"),
            Diff::insert(b"12"),
            Diff::equal(b"xy"),
            Diff::insert(b"34"),
            Diff::equal(b"z"),
            Diff::delete(b"cd"),
            Diff::insert(b"56"),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![
                Diff::delete("abxyzcd".as_bytes()),
                Diff::insert(b"12xy34z56"),
            ],
            diffs
        );

        let mut diffs = vec![
            Diff::delete(&['a', 'b']),
            Diff::insert(&['1', '2']),
            Diff::equal(&['x', 'y']),
            Diff::insert(&['3', '4']),
            Diff::equal(&['z']),
            Diff::delete(&['c', 'd']),
            Diff::insert(&['5', '6']),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![
                Diff::delete(&"abxyzcd".chars().collect::<Vec<_>>()[..]),
                Diff::insert(&"12xy34z56".chars().collect::<Vec<_>>()[..]),
            ],
            diffs
        );

        let mut diffs = vec![
            Diff::delete("🩵".as_bytes()),
            Diff::insert(b"12"),
            Diff::equal(b"xy"),
            Diff::insert("💨".as_bytes()),
            Diff::equal(b"z"),
            Diff::delete(b"cd"),
            Diff::insert(b"56"),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![
                Diff::delete("🩵xyzcd".as_bytes()),
                Diff::insert("12xy💨z56".as_bytes()),
            ],
            diffs
        );

        let mut diffs = vec![
            Diff::delete(&"🩵".chars().collect::<Vec<_>>()[..]),
            Diff::insert(&['1', '2']),
            Diff::equal(&['x', 'y']),
            Diff::insert(&"💨".chars().collect::<Vec<_>>()[..]),
            Diff::equal(&['z']),
            Diff::delete(&['c', 'd']),
            Diff::insert(&['5', '6']),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![
                Diff::delete(&"🩵xyzcd".chars().collect::<Vec<_>>()[..]),
                Diff::insert(&"12xy💨z56".chars().collect::<Vec<_>>()[..]),
            ],
            diffs
        );

        // High cost estimation
        dmp.set_edit_cost(5);
        let mut diffs = vec![
            Diff::delete(b"ab"),
            Diff::insert(b"12"),
            Diff::equal(b"wxyz"),
            Diff::delete(b"cd"),
            Diff::insert(b"34"),
        ];

        dmp.cleanup_efficiency(&mut diffs);
        assert_eq!(
            vec![
                Diff::delete("abwxyzcd".as_bytes()),
                Diff::insert(b"12wxyz34"),
            ],
            diffs
        );
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

        let diffs = vec![
            Diff::delete(&['a']),
            Diff::insert(&['1', '2', '3', '4']),
            Diff::equal(&['x', 'y', 'z']),
        ];
        assert_eq!(5, DiffMatchPatch::x_index(&diffs, 2));

        let diffs = vec![
            Diff::equal(&['a']),
            Diff::delete(&['1', '2', '3', '4']),
            Diff::equal(&['x', 'y', 'z']),
        ];
        assert_eq!(1, DiffMatchPatch::x_index(&diffs, 3));
    }

    #[test]
    fn test_diff_common_overlap() {
        // Detect any suffix/prefix overlap.
        // Null case
        assert_eq!(0, DiffMatchPatch::common_overlap(b"", b"abcd"));
        assert_eq!(
            0,
            DiffMatchPatch::common_overlap(&[], &"abcd".chars().collect::<Vec<_>>()[..])
        );

        // Whole case.
        assert_eq!(3, DiffMatchPatch::common_overlap(b"abc", b"abcd"));
        assert_eq!(
            3,
            DiffMatchPatch::common_overlap(
                &"abc".chars().collect::<Vec<_>>()[..],
                &"abcd".chars().collect::<Vec<_>>()[..]
            )
        );

        // No overlap.
        assert_eq!(0, DiffMatchPatch::common_overlap(b"123456", b"abcd"));
        assert_eq!(
            0,
            DiffMatchPatch::common_overlap(
                &"123456".chars().collect::<Vec<_>>()[..],
                &"abcd".chars().collect::<Vec<_>>()[..]
            )
        );

        // Overlap.
        assert_eq!(3, DiffMatchPatch::common_overlap(b"123456xxx", b"xxxabcd"));
        assert_eq!(
            3,
            DiffMatchPatch::common_overlap(
                &"123456xxx".chars().collect::<Vec<_>>()[..],
                &"xxxabcd".chars().collect::<Vec<_>>()[..]
            )
        );

        // Unicode.
        // Some overly clever languages (C#) may treat ligatures as equal to their
        // component letters.  E.g. U+FB01 == 'fi'
        assert_eq!(
            0,
            DiffMatchPatch::common_overlap(b"fi", "\u{7FFF}".as_bytes())
        );
        assert_eq!(
            0,
            DiffMatchPatch::common_overlap(
                &['f', 'i'],
                &"\u{7FFF}".chars().collect::<Vec<_>>()[..]
            )
        );

        // Complete overlap
        assert_eq!(
            6,
            DiffMatchPatch::common_overlap("❤️".as_bytes(), "❤️".as_bytes())
        );
        assert_eq!(
            2,
            DiffMatchPatch::common_overlap(
                &"❤️".chars().collect::<Vec<_>>()[..],
                &"❤️".chars().collect::<Vec<_>>()[..]
            )
        );

        // Partial unicode overlap
        assert_eq!(
            0,
            DiffMatchPatch::common_overlap("ஆ".as_bytes(), "இ".as_bytes())
        );
        assert_eq!(
            0,
            DiffMatchPatch::common_overlap(
                &"ஆ".chars().collect::<Vec<_>>()[..],
                &"இ".chars().collect::<Vec<_>>()[..]
            )
        );

        assert_eq!(
            3,
            DiffMatchPatch::common_overlap("ஆஇ".as_bytes(), "இ".as_bytes())
        );
        assert_eq!(
            1,
            DiffMatchPatch::common_overlap(
                &"ஆஇ".chars().collect::<Vec<_>>()[..],
                &"இ".chars().collect::<Vec<_>>()[..]
            )
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
            .half_match(
                &"1234567890".chars().collect::<Vec<_>>()[..],
                &"abcdef".chars().collect::<Vec<_>>()[..]
            )
            .is_none());
        assert!(dmp
            .half_match("12345".as_bytes(), "23".as_bytes())
            .is_none());
        assert!(dmp
            .half_match(
                &"12345".chars().collect::<Vec<_>>()[..],
                &"23".chars().collect::<Vec<_>>()[..]
            )
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
                prefix_long: &['1', '2'],
                suffix_long: &['9', '0'],
                prefix_short: &['a'],
                suffix_short: &['z'],
                common: &['3', '4', '5', '6', '7', '8']
            }),
            dmp.half_match(
                &"1234567890".chars().collect::<Vec<_>>()[..],
                &"a345678z".chars().collect::<Vec<_>>()[..]
            )
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
                prefix_long: &['a'],
                suffix_long: &['z'],
                prefix_short: &['1', '2'],
                suffix_short: &['9', '0'],
                common: &"345678".chars().collect::<Vec<_>>()[..]
            }),
            dmp.half_match(
                &"a345678z".chars().collect::<Vec<_>>()[..],
                &"1234567890".chars().collect::<Vec<_>>()[..]
            )
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
                prefix_long: &"abc".chars().collect::<Vec<_>>()[..],
                suffix_long: &"z".chars().collect::<Vec<_>>()[..],
                prefix_short: &"1234".chars().collect::<Vec<_>>()[..],
                suffix_short: &"0".chars().collect::<Vec<_>>()[..],
                common: &"56789".chars().collect::<Vec<_>>()[..]
            }),
            dmp.half_match(
                &"abc56789z".chars().collect::<Vec<_>>()[..],
                &"1234567890".chars().collect::<Vec<_>>()[..]
            )
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
        assert_eq!(
            Some(HalfMatch {
                prefix_long: &"a".chars().collect::<Vec<_>>()[..],
                suffix_long: &"xyz".chars().collect::<Vec<_>>()[..],
                prefix_short: &"1".chars().collect::<Vec<_>>()[..],
                suffix_short: &"7890".chars().collect::<Vec<_>>()[..],
                common: &"23456".chars().collect::<Vec<_>>()[..]
            }),
            dmp.half_match(
                &"a23456xyz".chars().collect::<Vec<_>>()[..],
                &"1234567890".chars().collect::<Vec<_>>()[..]
            )
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
                prefix_long: &"12123".chars().collect::<Vec<_>>()[..],
                suffix_long: &"123121".chars().collect::<Vec<_>>()[..],
                prefix_short: &"a".chars().collect::<Vec<_>>()[..],
                suffix_short: &"z".chars().collect::<Vec<_>>()[..],
                common: &"1234123451234".chars().collect::<Vec<_>>()[..]
            }),
            dmp.half_match(
                &"121231234123451234123121".chars().collect::<Vec<_>>()[..],
                &"a1234123451234z".chars().collect::<Vec<_>>()[..]
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
                prefix_long: &"".chars().collect::<Vec<_>>()[..],
                suffix_long: &"-=-=-=-=-=".chars().collect::<Vec<_>>()[..],
                prefix_short: &"x".chars().collect::<Vec<_>>()[..],
                suffix_short: &[],
                common: &"x-=-=-=-=-=-=-=".chars().collect::<Vec<_>>()[..]
            }),
            dmp.half_match(
                &"x-=-=-=-=-=-=-=-=-=-=-=-=".chars().collect::<Vec<_>>()[..],
                &"xx-=-=-=-=-=-=-=".chars().collect::<Vec<_>>()[..]
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
        assert_eq!(
            Some(HalfMatch {
                prefix_long: &"-=-=-=-=-=".chars().collect::<Vec<_>>()[..],
                suffix_long: &[],
                prefix_short: &[],
                suffix_short: &"y".chars().collect::<Vec<_>>()[..],
                common: &"-=-=-=-=-=-=-=y".chars().collect::<Vec<_>>()[..]
            }),
            dmp.half_match(
                &"-=-=-=-=-=-=-=-=-=-=-=-=y".chars().collect::<Vec<_>>()[..],
                &"-=-=-=-=-=-=-=yy".chars().collect::<Vec<_>>()[..]
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
        assert_eq!(
            Some(HalfMatch {
                prefix_long: &"qHillo".chars().collect::<Vec<_>>()[..],
                suffix_long: &"w".chars().collect::<Vec<_>>()[..],
                prefix_short: &"x".chars().collect::<Vec<_>>()[..],
                suffix_short: &"Hulloy".chars().collect::<Vec<_>>()[..],
                common: &"HelloHe".chars().collect::<Vec<_>>()[..]
            }),
            dmp.half_match(
                &"qHilloHelloHew".chars().collect::<Vec<_>>()[..],
                &"xHelloHeHulloy".chars().collect::<Vec<_>>()[..]
            )
        );

        // Optimal no halfmatch.
        dmp.set_timeout(None);
        assert!(dmp
            .half_match(
                &"qHilloHelloHew".chars().collect::<Vec<_>>()[..],
                &"xHelloHeHulloy".chars().collect::<Vec<_>>()[..]
            )
            .is_none());
        assert!(dmp
            .half_match(
                &"qHilloHelloHew".chars().collect::<Vec<_>>()[..],
                &"xHelloHeHulloy".chars().collect::<Vec<_>>()[..]
            )
            .is_none());
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

        let p = Patch {
            start1: 20,
            start2: 21,
            length1: 18,
            length2: 17,
            diffs: vec![
                Diff::equal(&"jump".chars().collect::<Vec<_>>()[..]),
                Diff::delete(&"s".chars().collect::<Vec<_>>()[..]),
                Diff::insert(&"ed".chars().collect::<Vec<_>>()[..]),
                Diff::equal(&" over ".chars().collect::<Vec<_>>()[..]),
                Diff::delete(&"the".chars().collect::<Vec<_>>()[..]),
                Diff::insert(&"a".chars().collect::<Vec<_>>()[..]),
                Diff::equal(&"\nlaz".chars().collect::<Vec<_>>()[..]),
            ],
        };
        assert_eq!(
            "@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n %0Alaz\n",
            p.to_string()
        );
    }

    #[test]
    fn test_patch_add_context() -> Result<(), Error> {
        let dmp = DiffMatchPatch::default();

        let mut ps = dmp.patch_from_text("@@ -21,4 +21,10 @@\n-jump\n+somersault\n")?;
        let p = ps.first_mut().unwrap();
        dmp.patch_add_context(p, b"The quick brown fox jumps over the lazy dog.");
        assert_eq!(
            "@@ -17,12 +17,18 @@\n fox \n-jump\n+somersault\n s ov\n",
            p.to_string()
        );

        let mut ps = dmp.patch_from_text("@@ -21,4 +21,10 @@\n-jump\n+somersault\n")?;
        let p = ps.first_mut().unwrap();
        dmp.patch_add_context(
            p,
            &"The quick brown fox jumps over the lazy dog."
                .chars()
                .collect::<Vec<_>>()[..],
        );
        assert_eq!(
            "@@ -17,12 +17,18 @@\n fox \n-jump\n+somersault\n s ov\n",
            p.to_string()
        );

        // Same, but not enough trailing context.
        let mut ps = dmp.patch_from_text("@@ -21,4 +21,10 @@\n-jump\n+somersault\n")?;
        let p = ps.first_mut().unwrap();
        dmp.patch_add_context(p, b"The quick brown fox jumps.");
        assert_eq!(
            "@@ -17,10 +17,16 @@\n fox \n-jump\n+somersault\n s.\n",
            p.to_string()
        );

        let mut ps = dmp.patch_from_text("@@ -21,4 +21,10 @@\n-jump\n+somersault\n")?;
        let p = ps.first_mut().unwrap();
        dmp.patch_add_context(
            p,
            &"The quick brown fox jumps.".chars().collect::<Vec<_>>()[..],
        );
        assert_eq!(
            "@@ -17,10 +17,16 @@\n fox \n-jump\n+somersault\n s.\n",
            p.to_string()
        );

        // Same, but not enough leading context.
        let mut ps = dmp.patch_from_text("@@ -3 +3,2 @@\n-e\n+at\n")?;
        let p = ps.first_mut().unwrap();
        dmp.patch_add_context(p, b"The quick brown fox jumps.");
        assert_eq!("@@ -1,7 +1,8 @@\n Th\n-e\n+at\n  qui\n", p.to_string());

        let mut ps = dmp.patch_from_text("@@ -3 +3,2 @@\n-e\n+at\n")?;
        let p = ps.first_mut().unwrap();
        dmp.patch_add_context(
            p,
            &"The quick brown fox jumps.".chars().collect::<Vec<_>>()[..],
        );
        assert_eq!("@@ -1,7 +1,8 @@\n Th\n-e\n+at\n  qui\n", p.to_string());

        // Same, but with ambiguity.
        let mut ps = dmp.patch_from_text("@@ -3 +3,2 @@\n-e\n+at\n")?;
        let p = ps.first_mut().unwrap();
        dmp.patch_add_context(
            p,
            b"The quick brown fox jumps.  The quick brown fox crashes.",
        );
        assert_eq!(
            "@@ -1,27 +1,28 @@\n Th\n-e\n+at\n  quick brown fox jumps. \n",
            p.to_string()
        );

        let mut ps = dmp.patch_from_text("@@ -3 +3,2 @@\n-e\n+at\n")?;
        let p = ps.first_mut().unwrap();
        dmp.patch_add_context(
            p,
            &"The quick brown fox jumps.  The quick brown fox crashes."
                .chars()
                .collect::<Vec<_>>()[..],
        );
        assert_eq!(
            "@@ -1,27 +1,28 @@\n Th\n-e\n+at\n  quick brown fox jumps. \n",
            p.to_string()
        );

        // Unicode?
        // Investigate: fails in both JS and Rust
        // let mut ps = dmp.patch_from_text("@@ -3 +3,2 @@\n-e\n+at\n")?;
        // let p = ps.first_mut().unwrap();
        // dmp.patch_add_context(
        //     p,
        //     &"The quick brown fox jumps🤩.  The quick brown fox crashes.".chars().collect::<Vec<_>>()[..],
        // );
        // assert_eq!(
        //     "@@ -1,27 +1,28 @@\n Th\n-e\n+at\n  quick brown fox jumps🤩\n\n",
        //     p.to_string()
        // );

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

        assert_eq!(
            Some((21, Some(4), 21, Some(10))),
            DiffMatchPatch::parse_patch_header(
                &"@@ -21,4 +21,10 @@".chars().collect::<Vec<_>>()[..]
            )
        );
        assert_eq!(
            Some((3, None, 3, Some(2))),
            DiffMatchPatch::parse_patch_header(&"@@ -3 +3,2 @@".chars().collect::<Vec<_>>()[..])
        );

        // Bad cases
        assert!(
            DiffMatchPatch::parse_patch_header(&"@@  +3,2 @@".chars().collect::<Vec<_>>()[..])
                .is_none()
        );
        assert!(DiffMatchPatch::parse_patch_header(
            &"@@ 2046 +3,2 @@".chars().collect::<Vec<_>>()[..]
        )
        .is_none());
    }

    #[test]
    fn test_patch_add_padding() -> Result<(), Error> {
        let dmp = DiffMatchPatch::default();
        // Both edges full.
        let mut patches = dmp.patch_make(PatchInput::Texts::<Efficient>("", "test"))?;
        assert_eq!("@@ -0,0 +1,4 @@\n+test\n", dmp.patch_to_text(&patches));

        dmp.patch_add_padding(&mut patches);
        assert_eq!(
            "@@ -1,8 +1,12 @@\n %01%02%03%04\n+test\n %01%02%03%04\n",
            dmp.patch_to_text(&patches)
        );

        let mut patches = dmp.patch_make(PatchInput::Texts::<Compat>("", "test"))?;
        assert_eq!("@@ -0,0 +1,4 @@\n+test\n", dmp.patch_to_text(&patches));

        dmp.patch_add_padding(&mut patches);
        assert_eq!(
            "@@ -1,8 +1,12 @@\n %01%02%03%04\n+test\n %01%02%03%04\n",
            dmp.patch_to_text(&patches)
        );

        // Both edges partial.
        let mut patches = dmp.patch_make(PatchInput::Texts::<Efficient>("XY", "XtestY"))?;
        assert_eq!(
            "@@ -1,2 +1,6 @@\n X\n+test\n Y\n",
            dmp.patch_to_text(&patches)
        );
        dmp.patch_add_padding(&mut patches);
        assert_eq!(
            "@@ -2,8 +2,12 @@\n %02%03%04X\n+test\n Y%01%02%03\n",
            dmp.patch_to_text(&patches)
        );

        let mut patches = dmp.patch_make(PatchInput::Texts::<Compat>("XY", "XtestY"))?;
        assert_eq!(
            "@@ -1,2 +1,6 @@\n X\n+test\n Y\n",
            dmp.patch_to_text(&patches)
        );
        dmp.patch_add_padding(&mut patches);
        assert_eq!(
            "@@ -2,8 +2,12 @@\n %02%03%04X\n+test\n Y%01%02%03\n",
            dmp.patch_to_text(&patches)
        );

        // Both edges none.
        let mut patches =
            dmp.patch_make(PatchInput::Texts::<Efficient>("XXXXYYYY", "XXXXtestYYYY"))?;
        assert_eq!(
            "@@ -1,8 +1,12 @@\n XXXX\n+test\n YYYY\n",
            dmp.patch_to_text(&patches)
        );
        dmp.patch_add_padding(&mut patches);
        assert_eq!(
            percent_encoding::percent_decode(b"@@ -5,8 +5,12 @@\n XXXX\n+test\n YYYY\n")
                .decode_utf8()
                .unwrap(),
            dmp.patch_to_text(&patches)
        );

        let mut patches =
            dmp.patch_make(PatchInput::Texts::<Compat>("XXXXYYYY", "XXXXtestYYYY"))?;
        assert_eq!(
            "@@ -1,8 +1,12 @@\n XXXX\n+test\n YYYY\n",
            dmp.patch_to_text(&patches)
        );
        dmp.patch_add_padding(&mut patches);
        assert_eq!(
            percent_encoding::percent_decode(b"@@ -5,8 +5,12 @@\n XXXX\n+test\n YYYY\n")
                .decode_utf8()
                .unwrap(),
            dmp.patch_to_text(&patches)
        );

        // Unicode
        let mut patches =
            dmp.patch_make(PatchInput::Texts::<Efficient>("XXXXYYYY", "XXXX🤩YYYY"))?;

        dmp.patch_add_padding(&mut patches);
        assert_eq!(
            "@@ -5,8 +5,12 @@\n XXXX\n+🤩\n YYYY\n",
            percent_encoding::percent_decode(dmp.patch_to_text(&patches).as_bytes())
                .decode_utf8()
                .unwrap()
        );

        let mut patches = dmp.patch_make(PatchInput::Texts::<Compat>("XXXXYYYY", "XXXX🤩YYYY"))?;

        dmp.patch_add_padding(&mut patches);
        assert_eq!(
            "@@ -5,8 +5,9 @@\n XXXX\n+🤩\n YYYY\n",
            percent_encoding::percent_decode(dmp.patch_to_text(&patches).as_bytes())
                .decode_utf8()
                .unwrap()
        );

        Ok(())
    }

    #[test]
    fn test_patch_split_max() -> Result<(), Error> {
        let dmp = DiffMatchPatch::default();

        // Assumes that dmp.Match_MaxBits is 32.
        let mut patches = dmp.patch_make(PatchInput::Texts::<Efficient>(
            "abcdefghijklmnopqrstuvwxyz01234567890",
            "XabXcdXefXghXijXklXmnXopXqrXstXuvXwxXyzX01X23X45X67X89X0",
        ))?;
        dmp.split_max(&mut patches);
        assert_eq!(
            "@@ -1,32 +1,46 @@\n+X\n ab\n+X\n cd\n+X\n ef\n+X\n gh\n+X\n ij\n+X\n kl\n+X\n mn\n+X\n op\n+X\n qr\n+X\n st\n+X\n uv\n+X\n wx\n+X\n yz\n+X\n 012345\n@@ -25,13 +39,18 @@\n zX01\n+X\n 23\n+X\n 45\n+X\n 67\n+X\n 89\n+X\n 0\n",
            dmp.patch_to_text(&patches)
        );

        let mut patches = dmp.patch_make(PatchInput::Texts::<Compat>(
            "abcdefghijklmnopqrstuvwxyz01234567890",
            "XabXcdXefXghXijXklXmnXopXqrXstXuvXwxXyzX01X23X45X67X89X0",
        ))?;
        dmp.split_max(&mut patches);
        assert_eq!(
            "@@ -1,32 +1,46 @@\n+X\n ab\n+X\n cd\n+X\n ef\n+X\n gh\n+X\n ij\n+X\n kl\n+X\n mn\n+X\n op\n+X\n qr\n+X\n st\n+X\n uv\n+X\n wx\n+X\n yz\n+X\n 012345\n@@ -25,13 +39,18 @@\n zX01\n+X\n 23\n+X\n 45\n+X\n 67\n+X\n 89\n+X\n 0\n",
            dmp.patch_to_text(&patches)
        );

        let mut patches = dmp.patch_make(PatchInput::Texts::<Efficient>(
            "abcdef1234567890123456789012345678901234567890123456789012345678901234567890uvwxyz",
            "abcdefuvwxyz",
        ))?;
        let p2t = dmp.patch_to_text(&patches);
        dmp.split_max(&mut patches);
        assert_eq!(p2t, dmp.patch_to_text(&patches));

        let mut patches = dmp.patch_make(PatchInput::Texts::<Compat>(
            "abcdef1234567890123456789012345678901234567890123456789012345678901234567890uvwxyz",
            "abcdefuvwxyz",
        ))?;

        let p2t = dmp.patch_to_text(&patches);
        dmp.split_max(&mut patches);
        assert_eq!(p2t, dmp.patch_to_text(&patches));

        let mut patches = dmp.patch_make(PatchInput::Texts::<Efficient>(
            "1234567890123456789012345678901234567890123456789012345678901234567890",
            "abc",
        ))?;
        dmp.split_max(&mut patches);
        assert_eq!(
            "@@ -1,32 +1,4 @@\n-1234567890123456789012345678\n 9012\n@@ -29,32 +1,4 @@\n-9012345678901234567890123456\n 7890\n@@ -57,14 +1,3 @@\n-78901234567890\n+abc\n",
            dmp.patch_to_text(&patches)
        );

        let p2t = dmp.patch_to_text(&patches);
        dmp.split_max(&mut patches);
        assert_eq!(p2t, dmp.patch_to_text(&patches));

        let mut patches = dmp.patch_make(PatchInput::Texts::<Compat>(
            "1234567890123456789012345678901234567890123456789012345678901234567890",
            "abc",
        ))?;
        dmp.split_max(&mut patches);
        assert_eq!(
            "@@ -1,32 +1,4 @@\n-1234567890123456789012345678\n 9012\n@@ -29,32 +1,4 @@\n-9012345678901234567890123456\n 7890\n@@ -57,14 +1,3 @@\n-78901234567890\n+abc\n",
            dmp.patch_to_text(&patches)
        );

        let mut patches = dmp.patch_make(PatchInput::Texts::<Efficient>(
            "abcdefghij , h : 0 , t : 1 abcdefghij , h : 0 , t : 1 abcdefghij , h : 0 , t : 1",
            "abcdefghij , h : 1 , t : 1 abcdefghij , h : 1 , t : 1 abcdefghij , h : 0 , t : 1",
        ))?;
        dmp.split_max(&mut patches);
        assert_eq!(
            "@@ -2,32 +2,32 @@\n bcdefghij , h : \n-0\n+1\n  , t : 1 abcdef\n@@ -29,32 +29,32 @@\n bcdefghij , h : \n-0\n+1\n  , t : 1 abcdef\n",
            dmp.patch_to_text(&patches)
        );

        let mut patches = dmp.patch_make(PatchInput::Texts::<Compat>(
            "abcdefghij , h : 0 , t : 1 abcdefghij , h : 0 , t : 1 abcdefghij , h : 0 , t : 1",
            "abcdefghij , h : 1 , t : 1 abcdefghij , h : 1 , t : 1 abcdefghij , h : 0 , t : 1",
        ))?;
        dmp.split_max(&mut patches);
        assert_eq!(
            "@@ -2,32 +2,32 @@\n bcdefghij , h : \n-0\n+1\n  , t : 1 abcdef\n@@ -29,32 +29,32 @@\n bcdefghij , h : \n-0\n+1\n  , t : 1 abcdef\n",
            dmp.patch_to_text(&patches)
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
        assert_eq!(
            HashMap::from([('a', 4), ('b', 2), ('c', 1)]),
            DiffMatchPatch::match_alphabet(&['a', 'b', 'c'])
        );

        // Duplicates.
        assert_eq!(
            HashMap::from([('a', 37), ('b', 18), ('c', 8)]),
            DiffMatchPatch::match_alphabet(&"abcaba".chars().collect::<Vec<_>>()[..])
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

        // dmp.`match_threshold` = 0.3;
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
}

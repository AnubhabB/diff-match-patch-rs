use std::{
    collections::HashMap,
    time::{Duration, Instant},
};

/// Enum representing the different ops of diff
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
#[derive(Debug, PartialEq, Eq)]
pub struct Diff(Ops, Vec<u8>);

impl Diff {
    /// Create a new diff object
    pub fn new(op: Ops, text: &[u8]) -> Self {
        Self(op, text.to_vec())
    }

    /// helper functions to create ops
    pub fn delete(text: &[u8]) -> Self {
        Self::new(Ops::Delete, text)
    }

    pub fn insert(text: &[u8]) -> Self {
        Self::new(Ops::Insert, text)
    }

    pub fn equal(text: &[u8]) -> Self {
        Self::new(Ops::Equal, text)
    }
}

pub struct Patch {}

pub type Patches = Vec<Patch>;

pub struct DiffMatchPatch {
    /// a speedup flag, If present and false, then don't run
    /// a line-level diff first to identify the changed areas.
    /// Defaults to true, which does a faster, slightly less optimal diff.
    checklines: Option<bool>,
    /// A default timeout in num milliseconda, defaults to 1000 (1 second)
    timeout: Option<u64>,
}

impl Default for DiffMatchPatch {
    fn default() -> Self {
        Self {
            checklines: Some(true),
            timeout: Some(1000),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct HalfMatch<'a> {
    prefix_long: &'a [u8],
    suffix_long: &'a [u8],
    prefix_short: &'a [u8],
    suffix_short: &'a [u8],
    common: &'a [u8],
}

impl DiffMatchPatch {
    fn checklines(&self) -> bool {
        self.checklines.map_or(true, |c| c)
    }

    // returns the configured timeout, defaults to `1`, None or `0` would mean infinite timeout
    fn timeout(&self) -> u64 {
        self.timeout
            .map_or(31536000_u64, |tout| if tout > 0 { tout } else { u64::MAX })
    }

    fn main_internal<'a>(
        &self,
        old_bytes: &'a [u8],
        new_bytes: &'a [u8],
        linemode: bool,
        deadline: Instant,
    ) -> Vec<Diff> {
        // First, check if lhs and rhs are equal
        if old_bytes == new_bytes {
            if old_bytes.is_empty() {
                return Vec::new();
            }

            return vec![Diff::equal(old_bytes)];
        }

        if old_bytes.is_empty() {
            return vec![Diff::insert(new_bytes)];
        }

        if new_bytes.is_empty() {
            return vec![Diff::delete(old_bytes)];
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
        );

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

        diffs
    }

    fn compute<'a>(&self, old: &'a [u8], new: &'a [u8], linemode: bool, deadline: Instant) -> Vec<Diff> {
        // returning all of the new part
        if old.is_empty() {
            return vec![Diff::insert(new)];
        }

        // return everything deleted
        if new.is_empty() {
            return vec![Diff::delete(old)];
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

            return diffs;
        }

        if short.len() == 1 {
            // After previous case, this can't be an equality
            return vec![Diff::delete(old), Diff::insert(new)];
        }

        // Check if the problem can be split in two
        if let Some(half_match) = self.half_match(old, new) {
            let old_a = half_match.prefix_long;
            let old_b = half_match.suffix_long;

            let new_a = half_match.prefix_short;
            let new_b = half_match.suffix_short;

            let mid_common = half_match.common;

            // Send both pairs off for separate processing.
            let mut diffs_a = self.main_internal(old_a, new_a, linemode, deadline);
            let mut diffs_b = self.main_internal(old_b, new_b, linemode, deadline);

            // Merge the results
            diffs_a.push(Diff::equal(mid_common));
            diffs_a.append(&mut diffs_b);

            return diffs_a;
        }

        if linemode && old.len() > 100 && new.len() > 100 {
            return self.line_mode(old, new, deadline);
        }

        self.bisect(old, new, deadline)
    }

    fn half_match<'a>(&self, old: &'a [u8], new: &'a [u8]) -> Option<HalfMatch<'a>> {
        // Don't risk returning a suboptimal diff when we have unlimited time
        if self.timeout() == u64::MAX {
            return None;
        }

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
    fn line_mode<'a>(&self, old: &'a [u8], new: &'a [u8], deadline: Instant) -> Vec<Diff> {
        let to_chars = Self::lines_to_chars(old, new);
        let mut diffs = self.main_internal(&to_chars.chars_old, &to_chars.chars_new, false, deadline);

        // Convert diffs back to text
        Self::chars_to_lines(&mut diffs[..], &to_chars.lines[..]);
        // Eliminate freak matches
        Self::cleanup_semantic(&mut diffs);

        // Rediff any replacement blocks, this time character-by-character.

        // Add a dummy entry at the end.
        diffs.push(Diff::equal(b""));
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
            match diffs[pointer].0 {
                Ops::Insert => {
                    insert_n += 1;
                    let mut data = diffs[pointer].1.to_vec();
                    insert_data.append(&mut data);
                }
                Ops::Delete => {
                    delete_n += 1;
                    let mut data = diffs[pointer].1.to_vec();
                    delete_data.append(&mut data);
                }
                Ops::Equal => {
                    // Upon reaching an equality, check for prior redundancies.
                    if delete_n >= 1 && insert_n >= 1 {
                        // Delete the offending records and add the merged ones.
                        let idxstart = pointer - delete_n - insert_n;
                        let idxend = idxstart + delete_n + insert_n;

                        diffs.drain(idxstart .. idxend);

                        pointer = idxstart;

                        let mut subdiffs = self.main_internal(&delete_data, &insert_data, false, deadline);
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

        diffs
    }

    // Find the 'middle snake' of a diff, split the problem in two
    // and return the recursively constructed diff.
    // See Myers 1986 paper: An O(ND) Difference Algorithm and Its Variations.
    fn bisect<'a>(&self, old: &'a [u8], new: &'a [u8], deadline: Instant) -> Vec<Diff> {
        let old_len = old.len() as i32;
        let new_len = new.len() as i32;

        let max_d = (old_len + new_len + 1) / 2;
        let v_offset = max_d as usize;
        let v_len = 2 * max_d;

        let mut v1 = vec![-1_i32; v_len as usize];
        let mut v2 = vec![-1_i32; v_len as usize];

        v1[v_offset + 1] = 0;
        v2[v_offset + 1] = 0;
        // println!("{v1:?}");
        // println!("{v2:?}");

        let delta = old_len - new_len;

        // If the total number of characters is odd, then the front path will collide
        // with the reverse path.
        let front = delta % 2 != 0;

        // println!("v_offset[{v_offset}] v_length[{v_len}] delta[{delta}] front[{front}]");

        // Offsets for start and end of k loop.
        // Prevents mapping of space beyond the grid.
        let mut k1start = 0;
        let mut k1end = 0;
        let mut k2start = 0;
        let mut k2end = 0;

        for d in 0..max_d {
            // println!("For d: {d} --------------------");
            // Bail out if deadline is reached.
            if Instant::now() > deadline {
                break;
            }

            // Walk the front path one step
            for k1 in (k1start - d..d - k1end + 1).step_by(2) {
                let k1_offset = (v_offset as i32 + k1) as usize;
                // println!("Loop 1 k1[{k1}] k1_offset[{k1_offset}]");
                let mut x1 = if k1 == -d || (k1 != d && v1[k1_offset - 1] < v1[k1_offset + 1]) {
                    // println!("Loop 1 Check offset[if] {}", k1_offset + 1);
                    v1[k1_offset + 1]
                } else {
                    // println!("Loop 1 Check offset[else] {}", k1_offset - 1);
                    v1[k1_offset - 1] + 1
                };

                let mut y1 = x1 - k1;
                while x1 < old_len && y1 < new_len && old[x1 as usize] == new[y1 as usize] {
                    x1 += 1;
                    y1 += 1;
                }

                v1[k1_offset] = x1;

                if x1 > old_len {
                    // ran off the right of the graph
                    k1end += 2;
                } else if y1 > new_len {
                    // Ran of the bottom of the graph
                    k1start += 2;
                } else if front {
                    let k2_offset = v_offset as i32 + delta - k1;
                    if k2_offset >= 0 && k2_offset < v_len && v2[k2_offset as usize] != -1 {
                        // Mirror x2 onto top-left coodinate system
                        let x2 = old_len - v2[k2_offset as usize];
                        if x1 >= x2 {
                            // Overlap detected
                            // println!("Loop 1 Overlap detected! {x1} {y1}");
                            return self.bisect_split(old, new, x1 as usize, y1 as usize, deadline);
                        }
                    }
                }
            }

            // println!("After loop 1 k1start[{k1start}] k1end[{k1end}]");

            // Walk the reverse path one step
            for k2 in (k2start - d..d - k2end + 1).step_by(2) {
                let k2_offset = (v_offset as i32 + k2) as usize;
                // println!("Loop 2 k2: {k2} k2_offset: {k2_offset}");

                let mut x2 = if k2 == -d || (k2 != d && v2[k2_offset - 1] < v2[k2_offset + 1]) {
                    // println!("Loop 2 Check offset[if] {}", k2_offset + 1);
                    v2[k2_offset + 1]
                } else {
                    // println!("Loop 2 Check offset[else] {}", k2_offset - 1);
                    v2[k2_offset - 1] + 1
                };

                let mut y2 = x2 - k2;
                while x2 < old_len
                    && y2 < new_len
                    && old[(old_len - x2) as usize - 1] == new[(new_len - y2) as usize - 1]
                {
                    x2 += 1;
                    y2 += 1;
                }

                v2[k2_offset] = x2;
                if x2 > old_len {
                    // Ran off the left of the graph
                    k2end += 2;
                } else if y2 > new_len {
                    // Ran off the top of the graph
                    k2start += 2;
                } else if !front {
                    let k1_offset = v_offset as i32 + delta - k2;
                    // println!("Loop 2 not front! k1_offset[{k1_offset}] v_length[{v_len}]");
                    if k1_offset >= 0 && k1_offset < v_len && v1[k1_offset as usize] != -1 {
                        let x1 = v1[k1_offset as usize];
                        let y1 = v_offset as i32 + x1 - k1_offset;

                        // Mirror x2 onto top-left coordinate system
                        x2 = old_len - x2;
                        // println!("Loop 2 not front inner: x1[{x1}] y1[{y1}] x2[{x2}]");
                        if x1 >= x2 {
                            // Overlap
                            // println!("Loop 2 Overlap detected! {x1} {y1}");
                            return self.bisect_split(old, new, x1 as usize, y1 as usize, deadline);
                        }
                    }
                }
            }
        }

        vec![Diff::delete(old), Diff::insert(new)]
    }

    fn bisect_split(
        &self,
        old: &[u8],
        new: &[u8],
        x: usize,
        y: usize,
        deadline: Instant,
    ) -> Vec<Diff> {
        let old_a = &old[..x];
        let new_a = &new[..y];

        let old_b = &old[x..];
        let new_b = &new[y..];

        // Compute both diffs serially.
        let mut diffs_a = self.main_internal(old_a, new_a, false, deadline);
        let mut diffs_b = self.main_internal(old_b, new_b, false, deadline);

        diffs_a.append(&mut diffs_b);

        diffs_a
    }

    // Does a substring of shorttext exist within longtext such that the substring
    // is at least half the length of longtext?
    //idx Start index of quarter length substring within longtext.
    fn half_match_i<'a>(long: &'a [u8], short: &'a [u8], idx: usize) -> Option<HalfMatch<'a>> {
        // Start with a 1/4 length substring at position i as a seed.

        let seed = &long[idx..idx + long.len() / 4];
        let mut j = 0;

        let mut best_common: &[u8] = &[];
        let mut best_long_a: &[u8] = &[];
        let mut best_long_b: &[u8] = &[];
        let mut best_short_a: &[u8] = &[];
        let mut best_short_b: &[u8] = &[];

        while let Some(pos) = &short[j..]
            .windows(seed.len())
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
    fn common_prefix(lhs: &[u8], rhs: &[u8], reverse: bool) -> usize {
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
    fn cleanup_semantic(diffs: &mut Vec<Diff>) {
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
            if diffs[pointer].0 == Ops::Equal {
                equalities.push(pointer);
                // Updating pre equality changes
                insert_len_pre = insert_len_post;
                delete_len_pre = delete_len_post;

                // Resetting post insertion changes to 0
                insert_len_post = 0;
                delete_len_post = 0;

                last_equality = Some(diffs[pointer].1.clone());
            } else {
                // Ops::Insert || Ops::Delete
                // Increasing changes of post_equality metrics
                if diffs[pointer].0 == Ops::Insert {
                    insert_len_post += diffs[pointer].1.len();
                } else {
                    delete_len_post += diffs[pointer].1.len();
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
            if diffs[pointer - 1].0 == Ops::Delete && diffs[pointer].0 == Ops::Insert {
                let delete = diffs[pointer - 1].1.to_vec();
                let insert = diffs[pointer].1.to_vec();

                let delete_thres = delete.len() / 2 + if delete.len() % 2 > 0 { 1 } else { 0 };
                let insert_thres = insert.len() / 2 + if insert.len() % 2 > 0 { 1 } else { 0 };

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
    fn cleanup_semantic_lossless(diffs: &mut Vec<Diff>) {
        let mut pointer = 1_usize;
        let mut difflen = diffs.len();

        // Intentionally ignore the first and last element (don't need checking).
        while difflen > 0 && pointer < difflen - 1 {
            // an edit surrounded by equalities
            if diffs[pointer - 1].0 == Ops::Equal && diffs[pointer + 1].0 == Ops::Equal {
                let mut equality_prev = diffs[pointer - 1].1.clone();
                let mut edit = diffs[pointer].1.clone();
                let mut equality_next = diffs[pointer + 1].1.clone();

                // Shift the edit as far left as possible
                let commonlen = Self::common_prefix(&equality_prev[..], &edit[..], true);
                if commonlen > 0 {
                    let mut common_prev = edit[edit.len() - commonlen..].to_vec();
                    let mut common_next = common_prev.clone();

                    equality_prev = equality_prev[..equality_prev.len() - commonlen].to_vec();
                    common_prev.append(&mut edit[..edit.len() - commonlen].to_vec());
                    edit = common_prev;

                    common_next.append(&mut equality_next.to_vec());
                    equality_next = common_next;
                }

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
                if diffs[pointer - 1].1 != best_equality_prev {
                    if !best_equality_prev.is_empty() {
                        diffs[pointer - 1].1 = best_equality_prev.to_vec();
                    } else {
                        diffs.remove(pointer - 1);
                        pointer -= 1;
                        difflen = diffs.len();
                    }

                    diffs[pointer].1 = best_edit.to_vec();

                    if !best_equality_next.is_empty() {
                        diffs[pointer + 1].1 = best_equality_next.to_vec();
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

        // println!("Non-alphanum: [{} {}] Whitespace: [{whitespace_1} {whitespace_2}] Linebreak: [{linebreak_1} {linebreak_2}] Blankline: [{blankline_1} {blankline_2}] Blanklinematch: [{} {}]", !char1.is_alphanumeric(), !char2.is_alphanumeric(), one.ends_with(b"\n"), (two.starts_with(b"\n") || two.starts_with(b"\r")));

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
    fn cleanup_merge(diffs: &mut Vec<Diff>) {
        // Push a dummy diff ... this triggers the equality as a last step
        diffs.push(Diff::equal(b""));

        let mut difflen = diffs.len();

        let mut pointer = 0_usize;

        let mut insert_n = 0;
        let mut delete_n = 0;

        let mut insert_data = vec![];
        let mut delete_data = vec![];

        // let mut commonlen = 0;

        while pointer < difflen {
            match diffs[pointer].0 {
                Ops::Insert => {
                    insert_n += 1;
                    let mut data = diffs[pointer].1.to_vec();
                    insert_data.append(&mut data);
                    pointer += 1;
                }
                Ops::Delete => {
                    delete_n += 1;
                    let mut data = diffs[pointer].1.to_vec();
                    delete_data.append(&mut data);
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
                                if tmpidx > 0 && diffs[tmpidx - 1].0 == Ops::Equal {
                                    let mut appenddata = insert_data[..commonlen].to_vec();
                                    diffs[tmpidx - 1].1.append(&mut appenddata);
                                } else {
                                    diffs.insert(0, Diff::equal(&insert_data[..commonlen]));
                                    pointer += 1;
                                    difflen = diffs.len();
                                }
                                insert_data = insert_data[commonlen..].to_vec();
                                delete_data = delete_data[commonlen..].to_vec();
                            }

                            // Factor out any common suffixies.
                            let commonlen =
                                Self::common_prefix(&insert_data[..], &delete_data[..], true);
                            if commonlen > 0 {
                                diffs[pointer].1 = [
                                    insert_data[insert_data.len() - commonlen..].to_vec(),
                                    diffs[pointer].1.to_vec(),
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
                    } else if pointer != 0 && diffs[pointer - 1].0 == Ops::Equal {
                        // Merge this equality with the previous one.
                        let mut to_merge = diffs[pointer].1.to_vec();
                        diffs[pointer - 1].1.append(&mut to_merge);
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
            if dl.1.is_empty() {
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
                if diff_prev.0 == Ops::Equal && diff_next.0 == Ops::Equal {
                    let substr_idx = if diff.1.len() >= diff_prev.1.len() {
                        diff.1.len() - diff_prev.1.len()
                    } else {
                        0
                    };
                    if diff.1[substr_idx..] == diff_prev.1 {
                        // Shift the edit over the previous equality.
                        let new_current_data =
                            [&diff_prev.1, &diff.1[..diff.1.len() - diff_prev.1.len()]].concat();
                        let new_next_data = [&diff_prev.1[..], &diff_next.1[..]].concat();

                        diffs[pointer].1 = new_current_data;
                        diffs[pointer + 1].1 = new_next_data;
                        diffs.remove(pointer - 1);
                        difflen = diffs.len();

                        changes = true;
                    } else if diff.1[..if diff_next.1.len() <= diff.1.len() {
                        diff_next.1.len()
                    } else {
                        diff.1.len()
                    }] == diff_next.1
                    {
                        // Shift the edit over the next equality.
                        let mut next_data = diffs[pointer + 1].1.to_vec();

                        diffs[pointer - 1].1.append(&mut next_data);
                        diffs[pointer].1 = [
                            &diffs[pointer].1[diffs[pointer + 1].1.len()..],
                            &diffs[pointer + 1].1[..],
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
}

#[derive(Debug, Eq, PartialEq)]
struct LineToChars<'a> {
    chars_old: Vec<u8>,
    chars_new: Vec<u8>,
    lines: Vec<&'a [u8]>,
}

impl DiffMatchPatch {
    fn lines_to_chars<'a>(old: &'a [u8], new: &'a [u8]) -> LineToChars<'a> {
        let mut lines: Vec<&'a [u8]> = vec![];
        let mut linehash: HashMap<&'a [u8], usize> = HashMap::new();

        // Allocate 2/3rds of the space for text1, the rest for text2.
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

    fn lines_to_chars_internal<'a>(
        text: &'a [u8],
        array: &mut Vec<&'a [u8]>,
        hash: &mut HashMap<&'a [u8], usize>,
        maxlines: usize,
    ) -> Vec<u8> {
        let take = maxlines - array.len();

        let mut lines = text.split_inclusive(|u| *u == b'\n').enumerate();
        let mut charlist = Vec::with_capacity(take + 1);

        let mut broke = None;
        let mut cursor = 0;

        for (idx, line) in lines.by_ref() {
            cursor += line.len();

            let entry = hash.entry(line).or_insert(array.len());
            // fresh insert
            if entry == &array.len() {
                array.push(line);
            }

            // We know the `maxlines = 65535`, this will never fail
            charlist.push(char::from_u32(*entry as u32).unwrap());

            if idx == take - 1 {
                broke = Some(idx);
                break;
            }
        }

        //
        if broke.is_some() {
            let line = &text[cursor..];
            let e = hash.entry(line).or_insert(array.len());
            array.push(line);

            charlist.push(char::from_u32(*e as u32).unwrap());
        }

        let chars: String = charlist.iter().collect::<String>();

        chars.as_bytes().to_vec()
    }

    fn chars_to_lines(diffs: &mut [Diff], lines: &[&[u8]]) {
        diffs.iter_mut().for_each(|d| {
            let chars = String::from_utf8(d.1.to_vec()).unwrap();
            // let mut txt = &[];
            let t = chars
                .chars()
                .map(|c| {
                    let idx: u32 = c.into();
                    *lines.get(idx as usize).unwrap()
                })
                .collect::<Vec<_>>()
                .concat();

            d.1 = t;
        });
    }
}

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
    pub fn diff_main<'a>(&self, old: &'a str, new: &'a str) -> Vec<Diff> {
        let deadline = Instant::now()
            .checked_add(Duration::from_millis(self.timeout()))
            .unwrap();

        self.main_internal(old.as_bytes(), new.as_bytes(), self.checklines(), deadline)
    }

    // Reduce the number of edits by eliminating semantically trivial equalities
    pub fn diff_cleanup_semantic(diffs: &mut [Diff]) {
        todo!()
    }

    pub fn diff_cleanup_efficiency(diffs: &mut [Diff]) {
        todo!()
    }

    pub fn diff_levenshtein(diffs: &[Diff]) -> u32 {
        todo!()
    }

    pub fn diff_pretty_html(diffs: &[Diff]) -> String {
        todo!()
    }

    pub fn match_main(text: &str, pattern: &str, loc: ()) -> () {
        todo!()
    }

    pub fn patch_make_text_text(text1: &str, text2: &str) -> Patches {
        todo!()
    }

    pub fn patch_make_diff(diffs: &[Diff]) -> Patches {
        todo!()
    }

    pub fn patch_make_text_diff(text1: &str, diffs: &[Diff]) -> Patches {
        todo!()
    }

    pub fn patch_to_text(patches: Patches) -> String {
        todo!()
    }

    pub fn patch_from_text(text: &str) -> Patches {
        todo!()
    }

    pub fn patch_apply(patches: &[Patch], text: &str) -> (String, ()) {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::time::{Duration, Instant};

    use crate::dmp::{Diff, HalfMatch, LineToChars};

    use super::{DiffMatchPatch, Ops};

    // const tests = [
    //     'testDiffIsDestructurable', // TODO
    //     'testDiffCleanupEfficiency',
    //     'testDiffPrettyHtml',
    //     'testDiffText',
    //     'testDiffDelta',
    //     'testDiffXIndex',
    //     'testDiffLevenshtein',
    //     'testMatchAlphabet',
    //     'testMatchBitap',
    //     'testMatchMain',
    //     'testPatchObj',
    //     'testPatchFromText',
    //     'testPatchToText',
    //     'testPatchAddContext',
    //     'testPatchMake',
    //     'testPatchSplitMax',
    //     'testPatchAddPadding',
    //     'testPatchApply'
    // ];

    #[test]
    fn test_diff_is_destructurable() {}

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
        dmp.timeout = Some(u64::MAX);
        assert!(dmp
            .half_match("qHilloHelloHew".as_bytes(), "xHelloHeHulloy".as_bytes())
            .is_none());
    }

    #[test]
    fn test_diff_lines_to_chars() {
        // Convert lines down to characters.
        assert_eq!(
            LineToChars {
                chars_old: [0_usize, 1, 0]
                    .iter()
                    .map(|i| char::from_u32(*i as u32).unwrap())
                    .collect::<String>()
                    .as_bytes()
                    .to_vec(),
                chars_new: [1_usize, 0, 1]
                    .iter()
                    .map(|i| char::from_u32(*i as u32).unwrap())
                    .collect::<String>()
                    .as_bytes()
                    .to_vec(),
                lines: vec![b"alpha\n", b"beta\n"]
            },
            DiffMatchPatch::lines_to_chars(b"alpha\nbeta\nalpha\n", b"beta\nalpha\nbeta\n")
        );
        assert_eq!(
            LineToChars {
                chars_old: "".as_bytes().to_vec(),
                chars_new: [0_usize, 1, 2, 2]
                    .iter()
                    .map(|i| char::from_u32(*i as u32).unwrap())
                    .collect::<String>()
                    .as_bytes()
                    .to_vec(),
                lines: vec![b"alpha\r\n", b"beta\r\n", b"\r\n"]
            },
            DiffMatchPatch::lines_to_chars(b"", b"alpha\r\nbeta\r\n\r\n\r\n")
        );
        assert_eq!(
            LineToChars {
                chars_old: [0_usize]
                    .iter()
                    .map(|i| char::from_u32(*i as u32).unwrap())
                    .collect::<String>()
                    .as_bytes()
                    .to_vec(),
                chars_new: [1_usize]
                    .iter()
                    .map(|i| char::from_u32(*i as u32).unwrap())
                    .collect::<String>()
                    .as_bytes()
                    .to_vec(),
                lines: vec![b"a", b"b"]
            },
            DiffMatchPatch::lines_to_chars(b"a", b"b")
        );

        // More than 256 to reveal any 8-bit limitations.
        const TLIMIT: usize = 300;
        let linestr = (0..TLIMIT).map(|i| format!("{i}\n")).collect::<Vec<_>>();
        let linelist: Vec<&[u8]> = (0..TLIMIT).map(|i| linestr[i].as_bytes()).collect();
        let charlist = (0..TLIMIT)
            .map(|i| char::from_u32(i as u32).unwrap())
            .collect::<String>();

        assert_eq!(
            LineToChars {
                chars_old: charlist.as_bytes().to_vec(),
                chars_new: String::new().as_bytes().to_vec(),
                lines: linelist
            },
            DiffMatchPatch::lines_to_chars(linestr.join("").as_bytes(), b"")
        );
    }

    #[test]
    fn test_diff_chars_to_lines() {
        // Convert chars up to lines.
        let d1 = [0_usize, 1, 0]
            .iter()
            .map(|i| char::from_u32(*i as u32).unwrap())
            .collect::<String>();
        let d2 = [1_usize, 0, 1]
            .iter()
            .map(|i| char::from_u32(*i as u32).unwrap())
            .collect::<String>();
        let mut diffs = [Diff::equal(d1.as_bytes()), Diff::insert(d2.as_bytes())];

        DiffMatchPatch::chars_to_lines(&mut diffs, &[b"alpha\n", b"beta\n"]);

        assert_eq!(
            [
                Diff::equal(b"alpha\nbeta\nalpha\n"),
                Diff::insert(b"beta\nalpha\nbeta\n")
            ],
            diffs
        );

        // More than 256 to reveal any 8-bit limitations.
        const TLIMIT: usize = 300;

        let charlist = (0..TLIMIT)
            .map(|i| char::from_u32(i as u32).unwrap())
            .collect::<String>();
        let linestr = (0..TLIMIT).map(|i| format!("{i}\n")).collect::<Vec<_>>();
        let linelist: Vec<&[u8]> = (0..TLIMIT).map(|i| linestr[i].as_bytes()).collect();

        let mut diffs = [Diff::delete(charlist.as_bytes())];
        DiffMatchPatch::chars_to_lines(&mut diffs, &linelist[..]);

        assert_eq!([Diff::delete(linestr.join("").as_bytes())], diffs);

        // More than 65536 to verify any 16-bit limitation.
        const ULIMIT: usize = 10;
        let linestr = (0..ULIMIT).map(|i| format!("{i}\n")).collect::<Vec<_>>();
        let lines = linestr.join("");
        let l2c = DiffMatchPatch::lines_to_chars(lines.as_bytes(), b"");

        let mut diffs = [Diff::insert(&l2c.chars_old)];
        DiffMatchPatch::chars_to_lines(&mut diffs, &l2c.lines);

        assert_eq!(lines.as_bytes(), diffs[0].1);
    }

    #[test]
    fn test_diff_cleanup_merge() {
        // Cleanup a messy diff.
        // Null case.
        let mut diffs = vec![];
        let test: Vec<Diff> = vec![];
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
        let test: Vec<Diff> = vec![];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // No elimination #1.
        let mut diffs = vec![
            Diff::delete(b"ab"),
            Diff::insert(b"cd"),
            Diff::equal(b"12"),
            Diff::delete(b"e"),
        ];
        let test: Vec<Diff> = vec![
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
        let test: Vec<Diff> = vec![
            Diff::delete(b"abc"),
            Diff::insert(b"ABC"),
            Diff::equal(b"1234"),
            Diff::delete(b"wxyz"),
        ];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // Simple elimination.
        let mut diffs = vec![Diff::delete(b"a"), Diff::equal(b"b"), Diff::delete(b"c")];
        let test: Vec<Diff> = vec![Diff::delete(b"abc"), Diff::insert(b"b")];
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
        let test: Vec<Diff> = vec![Diff::delete(b"abcdef"), Diff::insert(b"cdfg")];
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
        let test: Vec<Diff> = vec![Diff::delete(b"AB_AB"), Diff::insert(b"1A2_1A2")];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // Word boundaries.
        let mut diffs = vec![
            Diff::equal(b"The c"),
            Diff::delete(b"ow and the c"),
            Diff::equal(b"at."),
        ];
        let test: Vec<Diff> = vec![
            Diff::equal(b"The "),
            Diff::delete(b"cow and the "),
            Diff::equal(b"cat."),
        ];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // No overlap elimination.
        let mut diffs = vec![Diff::delete(b"abcxx"), Diff::insert(b"xxdef")];
        let test: Vec<Diff> = vec![Diff::delete(b"abcxx"), Diff::insert(b"xxdef")];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // Overlap elimination.
        let mut diffs = vec![Diff::delete(b"abcxxx"), Diff::insert(b"xxxdef")];
        let test: Vec<Diff> = vec![
            Diff::delete(b"abc"),
            Diff::equal(b"xxx"),
            Diff::insert(b"def"),
        ];
        DiffMatchPatch::cleanup_semantic(&mut diffs);
        assert_eq!(test, diffs);

        // Reverse overlap elimination.
        let mut diffs = vec![Diff::delete(b"xxxabc"), Diff::insert(b"defxxx")];
        let test: Vec<Diff> = vec![
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
        let test: Vec<Diff> = vec![
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
        let mut diffs: Vec<Diff> = vec![];
        let test: Vec<Diff> = vec![];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Blank lines.
        let mut diffs: Vec<Diff> = vec![
            Diff::equal(b"AAA\r\n\r\nBBB"),
            Diff::insert(b"\r\nDDD\r\n\r\nBBB"),
            Diff::equal(b"\r\nEEE"),
        ];
        let test: Vec<Diff> = vec![
            Diff::equal(b"AAA\r\n\r\n"),
            Diff::insert(b"BBB\r\nDDD\r\n\r\n"),
            Diff::equal(b"BBB\r\nEEE"),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Line boundaries.
        let mut diffs: Vec<Diff> = vec![
            Diff::equal(b"AAA\r\nBBB"),
            Diff::insert(b" DDD\r\nBBB"),
            Diff::equal(b" EEE"),
        ];
        let test: Vec<Diff> = vec![
            Diff::equal(b"AAA\r\n"),
            Diff::insert(b"BBB DDD\r\n"),
            Diff::equal(b"BBB EEE"),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Word boundaries.
        let mut diffs: Vec<Diff> = vec![
            Diff::equal(b"The c"),
            Diff::insert(b"ow and the c"),
            Diff::equal(b"at."),
        ];
        let test: Vec<Diff> = vec![
            Diff::equal(b"The "),
            Diff::insert(b"cow and the "),
            Diff::equal(b"cat."),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Alphanumeric boundaries.
        let mut diffs: Vec<Diff> = vec![
            Diff::equal(b"The-c"),
            Diff::insert(b"ow-and-the-c"),
            Diff::equal(b"at."),
        ];
        let test: Vec<Diff> = vec![
            Diff::equal(b"The-"),
            Diff::insert(b"cow-and-the-"),
            Diff::equal(b"cat."),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Hitting the start.
        let mut diffs: Vec<Diff> = vec![Diff::equal(b"a"), Diff::delete(b"a"), Diff::equal(b"ax")];
        let test: Vec<Diff> = vec![Diff::delete(b"a"), Diff::equal(b"aax")];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Hitting the end.
        let mut diffs: Vec<Diff> = vec![Diff::equal(b"xa"), Diff::delete(b"a"), Diff::equal(b"a")];
        let test: Vec<Diff> = vec![Diff::equal(b"xaa"), Diff::delete(b"a")];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);

        // Sentence boundaries.
        let mut diffs: Vec<Diff> = vec![
            Diff::equal(b"The xxx. The "),
            Diff::insert(b"zzz. The "),
            Diff::equal(b"yyy."),
        ];
        let test: Vec<Diff> = vec![
            Diff::equal(b"The xxx."),
            Diff::insert(b" The zzz."),
            Diff::equal(b" The yyy."),
        ];
        DiffMatchPatch::cleanup_semantic_lossless(&mut diffs);
        assert_eq!(test, diffs);
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
    fn test_diff_bisect() {
        let dmp = DiffMatchPatch::default();

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
            dmp.bisect(
                b"cat",
                b"map",
                Instant::now()
                    .checked_add(Duration::from_secs(600))
                    .unwrap()
            )
        );

        // Timeout.
        assert_eq!(
            vec![Diff::delete(b"cat"), Diff::insert(b"map"),],
            dmp.bisect(
                b"cat",
                b"map",
                Instant::now().checked_add(Duration::from_secs(0)).unwrap()
            )
        );
    }

    #[test]
    fn test_diff_main() {
        let mut dmp = DiffMatchPatch::default();

        // Perform a trivial diff.
        // Null case.
        assert!(dmp.diff_main("", "").is_empty());

        // Equality
        assert_eq!(vec![Diff::equal(b"abc")], dmp.diff_main("abc", "abc"));

        // Simple insert
        assert_eq!(
            vec![Diff::equal(b"ab"), Diff::insert(b"123"), Diff::equal(b"c")],
            dmp.diff_main("abc", "ab123c")
        );

        // Simple delete
        assert_eq!(
            vec![Diff::equal(b"a"), Diff::delete(b"123"), Diff::equal(b"bc")],
            dmp.diff_main("a123bc", "abc")
        );

        // Two insertions
        assert_eq!(
            vec![
                Diff::equal(b"a"),
                Diff::insert(b"123"),
                Diff::equal(b"b"),
                Diff::insert(b"456"),
                Diff::equal(b"c"),
            ],
            dmp.diff_main("abc", "a123b456c")
        );

        // Two deletions.
        assert_eq!(
            vec![
                Diff::equal(b"a"),
                Diff::delete(b"123"),
                Diff::equal(b"b"),
                Diff::delete(b"456"),
                Diff::equal(b"c"),
            ],
            dmp.diff_main("a123b456c", "abc")
        );

        // Perform a real diff.
        // Switch off the timeout.
        dmp.timeout = None;
        // Simple cases.
        assert_eq!(
            vec![Diff::delete(b"a"), Diff::insert(b"b"),],
            dmp.diff_main("a", "b")
        );

        assert_eq!(
            vec![
                Diff::delete(b"Apple"),
                Diff::insert(b"Banana"),
                Diff::equal(b"s are a"),
                Diff::insert(b"lso"),
                Diff::equal(b" fruit.")
            ],
            dmp.diff_main("Apples are a fruit.", "Bananas are also fruit.")
        );

        assert_eq!(
            vec![
                Diff::delete(b"a"),
                Diff::insert("\u{0680}".as_bytes()),
                Diff::equal(b"x"),
                Diff::delete(b"\t"),
                Diff::insert(b"\0")
            ],
            dmp.diff_main("ax\t", "\u{0680}x\0")
        );

        // Overlaps.
        assert_eq!(
            vec![
                Diff::delete(b"1"),
                Diff::equal(b"a"),
                Diff::delete(b"y"),
                Diff::equal(b"b"),
                Diff::delete(b"2"),
                Diff::insert(b"xab"),
            ],
            dmp.diff_main("1ayb2", "abxab")
        );

        assert_eq!(
            vec![
                Diff::insert(b"xaxcx"),
                Diff::equal(b"abc"),
                Diff::delete(b"y"),
            ],
            dmp.diff_main("abcy", "xaxcxabc")
        );

        assert_eq!(
            vec![
                Diff::delete(b"ABCD"),
                Diff::equal(b"a"),
                Diff::delete(b"="),
                Diff::insert(b"-"),
                Diff::equal(b"bcd"),
                Diff::delete(b"="),
                Diff::insert(b"-"),
                Diff::equal(b"efghijklmnopqrs"),
                Diff::delete(b"EFGHIJKLMNOefg"),
            ],
            dmp.diff_main("ABCDa=bcd=efghijklmnopqrsEFGHIJKLMNOefg", "a-bcd-efghijklmnopqrs")
        );

        // Large equality.
        assert_eq!(
            vec![
                Diff::insert(b" "),
                Diff::equal(b"a"),
                Diff::insert(b"nd"),
                Diff::equal(b" [[Hepatopancreatic]]"),
                Diff::delete(b" and [[New"),
            ],
            dmp.diff_main("a [[Hepatopancreatic]] and [[New", " and [[Hepatopancreatic]]")
        );

        // Timeout.
        const LOW_TIMEOUT: u64 = 100;
        dmp.timeout = Some(LOW_TIMEOUT);
        let a = vec!["`Twas brillig, and the slithy toves\nDid gyre and gimble in the wabe:\nAll mimsy were the borogoves,\nAnd the mome raths outgrabe.\n"; 2048].join("");
        let b = vec!["I am the very model of a modern major general,\nI\'ve information vegetable, animal, and mineral,\nI know the kings of England, and I quote the fights historical,\nFrom Marathon to Waterloo, in order categorical.\n"; 2048].join("");

        let start = Instant::now();
        dmp.diff_main(&a, &b);
        let end = Instant::now();
        // Test that we took at least the timeout period (+ 5ms being generous).
        assert!((end - start).as_millis() <= LOW_TIMEOUT as u128 + 5);

        // Test the linemode speedup.
        // Must be long to pass the 100 char cutoff.
        // Simple line-mode.
        let a =  "12345678901234567890123456789 0123456 78901234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n";
        let b = "abcdefghij abcdefghij abcdefghij abcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\n";
        dmp.checklines = Some(false);
        // let start = Instant::now();
        let res_no_lm = dmp.diff_main(a, b);
        // let no_lm = Instant::now() - start;
        dmp.checklines = Some(true);
        // let start = Instant::now();
        let res_yes_lm = dmp.diff_main(a, b);
        // let yes_lm = Instant::now() - start;

        // Now, we'll run 2 checks - one for result equality, two for speedup
        assert_eq!(res_no_lm, res_yes_lm);
        // Fails, no-linemode takes less time than with linemode optimizations
        // Off by a few 30μs
        // assert!(no_lm > yes_lm);

        // Single line-mode.
        let a = "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890";
        let b = "abcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghij";
        dmp.checklines = Some(true);
        let yes_lm = dmp.diff_main(a, b);
        dmp.checklines = Some(false);
        let no_lm = dmp.diff_main(a, b);
        assert_eq!(no_lm, yes_lm);

        // Overlap line-mode.
        let a = "1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n";
        let b = "abcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n";
        dmp.checklines = Some(true);
        let yes_lm = dmp.diff_main(a, b);
        dmp.checklines = Some(false);
        let no_lm = dmp.diff_main(a, b);
        assert_eq!(rebuild_text(&yes_lm[..]), rebuild_text(&no_lm[..]));

        // TODO
        //   // Test null inputs.
        //   try {
        //     dmp.diff_main(null, null);
        //     assertEquals(Error, null);
        //   } catch (e) {
        //     // Exception expected.
        //   }
    }

    // Helper to construct the two texts which made up the diff originally.
    fn rebuild_text(diffs: &[Diff]) -> (String, String) {
        let mut txt1 = vec![];
        let mut txt2 = vec![];

        diffs.iter()
        .for_each(|d| {
            let mut txt = d.1.clone();
            if d.0 != Ops::Insert {
                txt1.append(&mut txt);
            }

            let mut txt = d.1.clone();
            if d.0 != Ops::Delete {
                txt2.append(&mut txt);
            }
        });

        (String::from_utf8(txt1).unwrap(), String::from_utf8(txt2).unwrap())
    }
    // function diff_rebuildtexts(diffs) {
    //     
    //     var text1 = '';
    //     var text2 = '';
    //     for (var x = 0; x < diffs.length; x++) {
    //       if (diffs[x][0] != DIFF_INSERT) {
    //         text1 += diffs[x][1];
    //       }
    //       if (diffs[x][0] != DIFF_DELETE) {
    //         text2 += diffs[x][1];
    //       }
    //     }
    //     return [text1, text2];
    //   }
}

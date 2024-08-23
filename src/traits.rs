use chrono::NaiveTime;

use crate::dmp::{Diff, DiffMatchPatch};

pub trait BisectSplit: Copy + Ord + Eq {
    fn bisect_split(
        dmp: &DiffMatchPatch,
        old: &[Self],
        new: &[Self],
        x: usize,
        y: usize,
        deadline: Option<NaiveTime>,
    ) -> Result<Vec<Diff<Self>>, crate::errors::Error>;
}

impl BisectSplit for u8 {
    fn bisect_split(
        dmp: &DiffMatchPatch,
        old: &[u8],
        new: &[u8],
        x: usize,
        y: usize,
        deadline: Option<NaiveTime>
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
}

impl BisectSplit for usize {
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
}

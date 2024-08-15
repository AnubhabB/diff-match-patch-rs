use std::time::Instant;

use crate::dmp::{Diff, DiffMatchPatch};

pub(crate) trait BisectSplit: Copy + Ord + Eq {
    fn bisect_split(
        dmp: &DiffMatchPatch,
        old: &[Self],
        new: &[Self],
        x: usize,
        y: usize,
        deadline: Instant,
    ) -> Result<Vec<Diff<Self>>, crate::errors::Error>;
}

impl BisectSplit for u8 {
    fn bisect_split(
        dmp: &DiffMatchPatch,
        old: &[u8],
        new: &[u8],
        x: usize,
        y: usize,
        deadline: Instant,
    ) -> Result<Vec<Diff<u8>>, crate::errors::Error> {
        let old_a = &old[..x];
        let new_a = &new[..y];

        let old_b = &old[x..];
        let new_b = &new[y..];

        // Compute both diffs serially.
        let mut diffs_a = dmp.diff_internal(old_a, new_a, false, deadline)?;
        let mut diffs_b = dmp.diff_internal(old_b, new_b, false, deadline)?;

        diffs_a.append(&mut diffs_b);

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
        deadline: Instant,
    ) -> Result<Vec<Diff<usize>>, crate::errors::Error> {
        let old_a = &old[..x];
        let new_a = &new[..y];

        let old_b = &old[x..];
        let new_b = &new[y..];

        // Compute both diffs serially.
        let mut diffs_a = dmp.diff_lines(old_a, new_a, deadline)?;
        let mut diffs_b = dmp.diff_lines(old_b, new_b, deadline)?;

        diffs_a.append(&mut diffs_b);

        Ok(diffs_a)
    }
}

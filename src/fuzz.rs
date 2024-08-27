use crate::{Compat, DiffMatchPatch, Efficient, Error, PatchInput};

pub fn fuzz(old: &str, new: &str) -> Result<(), Error> {
    let dmp = DiffMatchPatch::new();

    let diffs = dmp.diff_main::<Efficient>(old, new)?;
    let patches = dmp.patch_make(PatchInput::new_diffs(&diffs))?;

    assert_eq!(new, dmp.patch_apply(&patches, old)?.0);

    let dmp = DiffMatchPatch::new();

    let diffs = dmp.diff_main::<Compat>(old, new)?;
    let patches = dmp.patch_make(PatchInput::new_diffs(&diffs))?;

    assert_eq!(new, dmp.patch_apply(&patches, old)?.0);

    Ok(())
}

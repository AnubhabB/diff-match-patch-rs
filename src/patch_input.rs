use crate::{dmp::Diff, DType};

pub enum PatchInput<'a, T: DType> {
    Texts(&'a str, &'a str),
    Diffs(&'a [Diff<T>]),
    TextDiffs(&'a str, &'a [Diff<T>]),
}

impl<'a, T: DType> PatchInput<'a, T> {
    pub fn new_text_text(old: &'a str, new: &'a str) -> Self {
        Self::Texts(old, new)
    }

    pub fn new_diffs(diffs: &'a [Diff<T>]) -> Self {
        Self::Diffs(diffs)
    }

    pub fn new_text_diffs(old: &'a str, diffs: &'a [Diff<T>]) -> Self {
        Self::TextDiffs(old, diffs)
    }
}

use serde_repr::{Deserialize_repr, Serialize_repr};

/// Enum representing the different ops of diff
#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize_repr, Deserialize_repr)]
#[repr(i8)]
pub enum Ops {
    Delete = -1,
    Insert,
    Equal,
}

/// A trait defining Diff operations
pub trait DiffTrait<T: Sized + Copy + Ord + Eq> {
    fn new(op: Ops, lines: &[T]) -> Self;

    /// helper functions to create ops
    fn delete(lines: &[T]) -> Self;

    fn insert(lines: &[T]) -> Self;

    fn equal(lines: &[T]) -> Self;

    // returns the operation of the current diff
    fn op(&self) -> Ops;

    // returns the inner data
    fn data(&self) -> &[T];
}
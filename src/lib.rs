#![feature(trait_alias, let_chains)]

pub mod dmp;
pub mod errors;
pub mod traits;

pub(crate) use traits::DType;
pub use dmp::{DiffMatchPatch, Ops, Patch, PatchInput, Patches};
pub use errors::Error;

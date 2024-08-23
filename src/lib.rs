pub mod dmp;
pub mod errors;
pub mod traits;

pub use dmp::{DiffMatchPatch, Ops, Patch, PatchInput, Patches};
pub use errors::Error;

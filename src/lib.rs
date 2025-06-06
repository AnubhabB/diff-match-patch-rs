#![doc = include_str!("../README.md")]

pub mod dmp;
pub mod errors;
pub mod fuzz;
pub mod html;
pub mod patch_input;
pub mod traits;

pub use dmp::{DiffMatchPatch, Ops, Patch, Patches};
pub use errors::Error;
pub use html::HtmlConfig;
pub use patch_input::PatchInput;
pub(crate) use traits::DType;
pub use traits::{Compat, Efficient};

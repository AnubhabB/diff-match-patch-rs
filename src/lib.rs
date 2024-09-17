//! # Efficient port of Google's diff-match-patch implemented in Rust
//!
//! [<img alt="github" src="https://img.shields.io/badge/github-Anubhab/diff_match_patch_rs-8da0cb?style=for-the-badge&labelColor=555555&logo=github" height="20">](https://github.com/AnubhabB/diff-match-patch-rs)
//! [<img alt="crates.io" src="https://img.shields.io/crates/v/diff-match-patch-rs" height="20">](https://crates.io/crates/diff-match-patch-rs)
//! [<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-diff_match_patch_rs?style=for-the-badge&logo=docs.rs&labelColor=%23555555" height="20">](https://docs.rs/diff-match-patch-rs)
//!
//!
//! A very **fast**, **accurate** and **wasm ready** port of [Diff Match Patch](https://github.com/dmsnell/diff-match-patch) in Rust. The
//! diff implementation is based on [Myers' diff algorithm](https://neil.fraser.name/writing/diff/myers.pdf).
//!
//! ## Highlights of this crate
//! - Exposes two modes of operating with `diff-match-patch`, a `Efficient` mode and `Compat` mode. While the `Efficient` mode squeezes out the max performance the `Compat` mode ensures compatibility across other libraries or implementations (rust or otherwise). According to [Benchmarks](#benchmarks), our slower `Compat` mode is still faster than other implementations in rust.
//!     - **`Efficient`** mode works on `&[u8]` and the generated diffs break compatibility with other implementation. Use the **`Efficient`** mode ONLY if you are using this [crate](https://crates.io/crates/diff-match-patch-rs) at the source of diff generation and the destination.
//!     - **`Compat`** mode on the other hand works on `&[char]` and the generated `diffs` and `patches` are compatible across other implementations of `diff-match-patch`. Checkout `tests/compat.rs` for some test cases around this.
//! -  `wasm` ready, you can check out a [demo here](https://github.com/AnubhabB/wasm-diff.git)
//! - **Accurate**, while working on this crate I've realized that there are a bunch of implementations that have major issues (wrong diffs, inaccurate flows, silent errors etc.).
//! - Helper method **pretty_html** provided by this crate allows some configurations to control the generated visuals elements.
//! - Well tested
//! - Added a `fuzzer` for sanity
//! - Exposes the same APIs as [Diff Match Patch](https://github.com/dmsnell/diff-match-patch) with minor changes to make it more idiomatic in Rust.
//!
//! ## Usage Examples
//!
//! ```toml
//! [dependencies]
//! diff-match-patch-rs = "0.3.0"
//! ```
//!
//! ### `Effitient` mode
//!
//! ```rust
//! use diff_match_patch_rs::{DiffMatchPatch, Efficient, Error, PatchInput};
//!
//! // This is the source text
//! const TXT_OLD: &str = "I am the very model of a modern Major-General, I've information on vegetable, animal, and mineral, 🚀👏👀";
//!
//! // Let's assume this to be the text that was editted from the source text
//! const TXT_NEW: &str = "I am the very model of a cartoon individual, My animation's comical, unusual, and whimsical.😊👀";
//!
//! // An example of a function that creates a diff and returns a set of patches serialized
//! fn at_source() -> Result<String, Error> {
//!     // initializing the module
//!     let dmp = DiffMatchPatch::new();
//!     // create a list of diffs
//!     let diffs = dmp.diff_main::<Efficient>(TXT_OLD, TXT_NEW)?;
//!     // Now, we are going to create a list of `patches` to be applied to the old text to get the new text
//!     let patches = dmp.patch_make(PatchInput::new_diffs(&diffs))?;
//!     // in the real world you are going to transmit or store this diff serialized to undiff format to be consumed or used somewhere elese
//!     let patch_txt = dmp.patch_to_text(&patches);
//!
//!     Ok(patch_txt)
//! }
//!
//! fn at_destination(patches: &str) -> Result<(), Error> {
//!     // initializing the module
//!     let dmp = DiffMatchPatch::new();
//!     // lets recreate the diffs from patches
//!     let patches = dmp.patch_from_text::<Efficient>(patches)?;
//!     // Now, lets apply these patches to the `old_txt` which is the original to get the new text
//!     let (new_txt, ops) = dmp.patch_apply(&patches, TXT_OLD)?;
//!     // Lets print out if the ops succeeded or not
//!     ops.iter()
//!         .for_each(|&o| println!("{}", if o { "OK" } else { "FAIL" }));
//!
//!     // If everything goes as per plan you should see
//!     // OK
//!     // OK
//!     // ... and so on
//!
//!     // lets check out if our `NEW_TXT` (presumably the edited one)
//!     if new_txt != TXT_NEW {
//!         return Err(Error::InvalidInput);
//!     }
//!
//!     println!("Wallah! Patch applied successfully!");
//!
//!     Ok(())
//! }
//!
//! fn main() -> Result<(), Error> {
//!     // At the source of diff where the old text is being edited we'll create a set of patches
//!     let patches = at_source()?;
//!     // We'll send this diff to some destination e.g. db or the client where these changes are going to be applied
//!     // The destination will receive the patch string and will apply the patches to recreate the edits
//!     at_destination(&patches)
//! }
//!
//! ```
//!
//! ### `Compat` mode
//!
//! ```rust
//! use diff_match_patch_rs::{DiffMatchPatch, Compat, Error, PatchInput};
//!
//! // This is the source text
//! const TXT_OLD: &str = "I am the very model of a modern Major-General, I've information on vegetable, animal, and mineral, 🚀👏👀";
//!
//! // Let's assume this to be the text that was editted from the source text
//! const TXT_NEW: &str = "I am the very model of a cartoon individual, My animation's comical, unusual, and whimsical.😊👀";
//!
//! // An example of a function that creates a diff and returns a set of patches serialized
//! fn at_source() -> Result<String, Error> {
//!     // initializing the module
//!     let dmp = DiffMatchPatch::new();
//!     // create a list of diffs
//!     let diffs = dmp.diff_main::<Compat>(TXT_OLD, TXT_NEW)?;
//!     // Now, we are going to create a list of `patches` to be applied to the old text to get the new text
//!     let patches = dmp.patch_make(PatchInput::new_diffs(&diffs))?;
//!     // in the real world you are going to transmit or store this diff serialized to undiff format to be consumed or used somewhere elese
//!     let patch_txt = dmp.patch_to_text(&patches);

//!     Ok(patch_txt)
//! }
//!
//! fn at_destination(patches: &str) -> Result<(), Error> {
//!     // initializing the module
//!     let dmp = DiffMatchPatch::new();
//!     // lets recreate the diffs from patches
//!     let patches = dmp.patch_from_text::<Compat>(patches)?;
//!     // Now, lets apply these patches to the `old_txt` which is the original to get the new text
//!     let (new_txt, ops) = dmp.patch_apply(&patches, TXT_OLD)?;
//!     // Lets print out if the ops succeeded or not
//!     ops.iter()
//!         .for_each(|&o| println!("{}", if o { "OK" } else { "FAIL" }));
//!
//!     // If everything goes as per plan you should see
//!     // OK
//!     // OK
//!     // ... and so on
//!
//!     // lets check out if our `NEW_TXT` (presumably the edited one)
//!     if new_txt != TXT_NEW {
//!         return Err(Error::InvalidInput);
//!     }
//!
//!     println!("Wallah! Patch applied successfully!");
//!
//!     Ok(())
//! }
//!
//! fn main() -> Result<(), Error> {
//!     // At the source of diff where the old text is being edited we'll create a set of patches
//!     let patches = at_source()?;
//!     // We'll send this diff to some destination e.g. db or the client where these changes are going to be applied
//!     // The destination will receive the patch string and will apply the patches to recreate the edits
//!     at_destination(&patches)
//! }
//! ```
//! ### `Match` - fuzzy match of pattern in Text
//!
//! ```rust
//! use diff_match_patch_rs::{DiffMatchPatch, Compat, Error, PatchInput};
//! // This is the source text
//! const TXT: &str = "I am the very model of a modern Major-General, I've information on vegetable, animal, and mineral, 🚀👏👀";
//!
//! // The patter we are trying to fing
//! const PATTERN: &str = " that berry ";
//!
//! // Returns `location` of match if found, `None` if not found
//! fn main() -> Option<usize> {
//! let dmp = DiffMatchPatch::new();
//!
//! // works with both `Efficient` and `Compat` modes
//! // `5` here is an approx location to find `nearby` matches
//! dmp.match_main::<Efficient>(TXT, PATTERN, 5) // this should return Some(4)
//! }
//! ```
//!
//! #### Note
//! The `Efficient` and `Compat` mode APIs are identical with the only chage being the `generic` parameter declared during the calls.
//!
//! E.g. we initiated a `diff` in the `Efficient` mode with `dmp.diff_main::<Efficient>( ... )` while for `Compat` mode we did `dmp.diff_main::<Compat>( ... )`.
//!
//! Please checkout the `examples` directory of the [source repo](https://github.com/AnubhabB/diff-match-patch-rs/tree/main/examples) for a few common use-cases.
//!
//! <div class="warning">The `Effitient` and `Compat` modes are mutually exclusive and will not generate correct output if used interchangibly at source and destination</div>
//!
//! ## Benchmarks
//! Benchmarks are maintained [diff-match-patch-bench repository](https://github.com/AnubhabB/diff-match-patch-rs-bench)
//!
//! | Lang.   | Library                                                                                  | Diff Avg. | Patch Avg. | Bencher    | Mode        | Correct |
//! |:-------:|:----------------------------------------------------------------------------------------:|:---------:|:----------:|:----------:|:-----------:|:-------:|
//! | `rust`  | [diff_match_patch v0.1.1](https://crates.io/crates/diff_match_patch)[^2]                 | 68.108 ms | 10.596 ms  | Criterion  | -           |    ✅   |
//! | `rust`  | [dmp v0.2.0](https://crates.io/crates/dmp)                                               | 69.019 ms | 14.654 ms  | Criterion  | -           |    ✅   |
//! | `rust`  | [diff-match-patch-rs](https://github.com/AnubhabB/diff-match-patch-rs.git)<sup>our</sup> | 64.66 ms  | 631.13 µs  | Criterion  | `Efficient` |    ✅   |
//! | `rust`  | [diff-match-patch-rs](https://github.com/AnubhabB/diff-match-patch-rs.git)<sup>our</sup> | 64.68 ms  | 1.1703 ms  | Criterion  | `Compat`    |    ✅   |
//! | `go`    | [go-diff](https://github.com/sergi/go-diff)                                              | 50.31 ms  | 135.2 ms   | go test    | -           |    ✅   |
//! | `node`  | [diff-match-patch](https://www.npmjs.com/package/diff-match-patch)[^1]                   | 246.90 ms | 1.07 ms    | tinybench  | -           |    ❌   |
//! | `python`| [diff-match-patch](https://pypi.org/project/diff-match-patch/)                           | 1.01 s    | 0.25 ms    | timeit     | -           |    ✅   |
//!
//! [^1]: [diff-match-patch](https://www.npmjs.com/package/diff-match-patch) generated `patch text` and `delta` breaks on `unicode surrogates`.
//! [^2]: Adds an extra clone to the iterator because the `patch_apply` method takes mutable refc. to `diffs`.
//!
//!
//! ## Gotchas
//! **Diff incompatibility with `JavaScript` libs**:
//!
//! There are 2 kinds of implementations - one which use a `postprocessing` function for merging `unicode surrogates` which break compatibility with every other popular `diff-match-patch` implementations and the other kind (packages based on the original implementation) break while `urlEncode()` of unicode surrogates.
//! As of now, this crate brakes compatibility while working with `JS` generated diffs with the surrogate patch.
//! If you are interfacing with `JavaScript` in browser, using this crate through `wasm` would be ideal.
//!

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

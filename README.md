# diff-match-patch-rs: Efficient port of Google's diff-match-patch implemented in Rust

This library is a port of the [Diff Match Patch](https://github.com/dmsnell/diff-match-patch) to Rust. The
diff implementation is based on [Myers' diff algorithm](https://neil.fraser.name/writing/diff/myers.pdf).

## What's different in this implementation?
- Instead of `String` or `Vec<Char>` this library works with `&[u8]` avoiding allocation as much as possible, this in-turn provides significant performance boost [See Benchmarks](#benchmarks)
- The **line diff** speedup follows a slightly more efficient execution path

## Benchmarks
Benchmarks are maintained [diff-match-patch-bench repository](https://github.com/AnubhabB/diff-match-patch-rs-bench)

[TODO] Add benchmarks


## Related projects

Diff Match Patch was originally built in 2006 to power Google Docs.
- [Diff Match Patch](https://github.com/google/diff-match-patch) (and it's [fork](https://github.com/dmsnell/diff-match-patch))
- **Rust**: [Distil.io diff_match_patch](https://crates.io/crates/diff_match_patch)
- **Rust**: [dmp](https://crates.io/crates/dmp)
- **Rust**: [Dissimilar](https://crates.io/crates/dissimilar) by the awesome [David Tolnay](https://github.com/dtolnay)
- **Rust**: [diff_match_patch](https://crates.io/crates/diff_match_patch)
- **Rust**: [diffmatchpatch](https://crates.io/crates/diffmatchpatch)



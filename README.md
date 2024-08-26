# diff-match-patch-rs: Efficient port of Google's diff-match-patch implemented in Rust

This library is a port of the [Diff Match Patch](https://github.com/dmsnell/diff-match-patch) to Rust. The
diff implementation is based on [Myers' diff algorithm](https://neil.fraser.name/writing/diff/myers.pdf).

# NOT FOR PRODUCTION (YET!)

Please note, this experiment has a fundamental flaw! Working with `&[u8]` if the source and destination libraries both work with `rust` or can represent `String` as `Uint8Array`. Which means, because we are working with `&[u8]` we are losing interoperatibility between libraries or other implementations of DiffMatchPatch.


## What's different in this implementation?
- Instead of `String` or `Vec<Char>` this library works with `&[u8]` avoiding allocation as much as possible, this in-turn provides significant performance boost [See Benchmarks](#benchmarks)
- The **line diff** speedup follows a slightly more efficient execution path

## Benchmarks
Benchmarks are maintained [diff-match-patch-bench repository](https://github.com/AnubhabB/diff-match-patch-rs-bench)

# diff-match-patch-rs-bench
Benchmarking the crate `diff-match-patch-rs` against other implementations.

## `Diff`
| Lang.   | Library   | Avg. (ms)   | High (ms)   | Low (ms)   | Bencher   | Mode |
|:-------:|:---------:|:-----------:|:-----------:|:----------:|:---------:|:-----:
| `rust`  | [diff_match_patch v0.1.1](https://crates.io/crates/diff_match_patch) | 68.108 | 68.178 | 68.062 | Criterion | - |
| `rust`  | [diffmatchpatch v0.0.4](https://crates.io/crates/diffmatchpatch) | 66.454 | 66.476 | 66.434 | Criterion | - |
| `rust`  | [dmp v0.2.0](https://crates.io/crates/dmp) | 69.019 | 66.476 | 68.991 | Criterion | - |
| `rust`  | [diff-match-patch-rs](https://github.com/AnubhabB/diff-match-patch-rs.git)<sup>our</sup> | 65.487 | 65.519 | 65.458 | Criterion | `Efficient` |
| `rust`  | [diff-match-patch-rs](https://github.com/AnubhabB/diff-match-patch-rs.git)<sup>our</sup> | 65.642 | 65.667 | 65.621 | Criterion | `Compat` |

>
> Note:
> Omitting [dissimilar](https://crates.io/crates/dissimilar) from the results, I believe that crate has different goals and a headon benchmark is not fair
> Results: Avg[197.30] High[197.46] Low[197.19]


## Related projects

Diff Match Patch was originally built in 2006 to power Google Docs.
- [Diff Match Patch](https://github.com/google/diff-match-patch) (and it's [fork](https://github.com/dmsnell/diff-match-patch))
- **Rust**: [Distil.io diff_match_patch](https://crates.io/crates/diff_match_patch)
- **Rust**: [dmp](https://crates.io/crates/dmp)
- **Rust**: [Dissimilar](https://crates.io/crates/dissimilar) by the awesome [David Tolnay](https://github.com/dtolnay)
- **Rust**: [diff_match_patch](https://crates.io/crates/diff_match_patch)
- **Rust**: [diffmatchpatch](https://crates.io/crates/diffmatchpatch)



# diff-match-patch-rs: Efficient port of Google's diff-match-patch implemented in Rust
======================================================================================

This library is a port of the [Diff Match Patch](https://github.com/dmsnell/diff-match-patch) to Rust. The
diff implementation is based on [Myers' diff algorithm](https://neil.fraser.name/writing/diff/myers.pdf).

## What's different in this implementation?
- Instead of `String` or `Vec<Char>` this library works with `&[u8]` avoiding allocation as much as possible, this in-turn provides significant performance boost [See Benchmarks](#benchmarks)
- The **line diff** speedup follows a slightly more efficient execution path

## Benchmarks
Benchmarks are maintained [diff-match-patch-bench repository](https://github.com/AnubhabB/diff-match-patch-rs-bench)
[TODO] Add benchmarks

Diff Match Patch was originally built in 2006 to power Google Docs.

[Diff Match Patch]: https://github.com/google/diff-match-patch (and it's fork https://github.com/dmsnell/diff-match-patch)
[Myers' diff algorithm]: https://neil.fraser.name/writing/diff/myers.pdf
[semantic cleanups]: https://neil.fraser.name/writing/diff/


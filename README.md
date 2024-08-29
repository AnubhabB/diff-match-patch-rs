# diff-match-patch-rs: Efficient port of Google's diff-match-patch implemented in Rust

A very **fast**, **accurate** and **wasm ready** port of [Diff Match Patch](https://github.com/dmsnell/diff-match-patch) in Rust. The diff implementation is based on [Myers' diff algorithm](https://neil.fraser.name/writing/diff/myers.pdf).

## Highlights of this crate
- Exposes two modes of operating with `diff-match-patch`, a `Efficient` mode and `Compat` mode. While the `Efficient` mode squeezes out the max performance the `Compat` mode ensures compatibility across other libraries or implementations (rust or otherwise). According to [Benchmarks](#benchmarks), our slower `Compat` mode is still faster than other implementations in rust.
    - **`Efficient`** mode works on `&[u8]` and the generated diffs break compatibility with other implementation. Use the **`Efficient`** mode ONLY if you are using this [crate](https://crates.io/crates/diff-match-patch-rs) at the source of diff generation and the destination.
    - **`Compat`** mode on the other hand works on `&[char]` and the generated `diffs` and `patches` are compatible across other implementations of `diff-match-patch`. Checkout `tests/compat.rs` for some test cases around this.
-  `wasm` ready, you can check out a [demo here](https://github.com/AnubhabB/wasm-diff.git)
- **Accurate**, while working on this crate I've realized that there are a bunch of implementations that have major issues (wrong diffs, inaccurate flows, silent errors etc.).
- Helper method **pretty_html** provided by this crate allows some configurations to control the generated visuals elements.
- Well tested
- Added a `fuzzer` for sanity
- Exposes the same APIs as [Diff Match Patch](https://github.com/dmsnell/diff-match-patch) with minor changes to make it more idiomatic in Rust.

## Usage Examples

```toml
[dependencies]
diff-match-patch-rs = "0.1.0-beta.3"
```

### `Effitient` mode

```rust
use diff_match_patch_rs::{DiffMatchPatch, Efficient, Error, PatchInput};

// This is the source text
const TXT_OLD: &str = "I am the very model of a modern Major-General, I've information on vegetable, animal, and mineral, 🚀👏👀";

// Let's assume this to be the text that was editted from the source text
const TXT_NEW: &str = "I am the very model of a cartoon individual, My animation's comical, unusual, and whimsical.😊👀";

// An example of a function that creates a diff and returns a set of patches serialized
fn at_source() -> Result<String, Error> {
    // initializing the module
    let dmp = DiffMatchPatch::new();
    // create a list of diffs
    let diffs = dmp.diff_main::<Efficient>(TXT_OLD, TXT_NEW)?;
    // Now, we are going to create a list of `patches` to be applied to the old text to get the new text
    let patches = dmp.patch_make(PatchInput::new_diffs(&diffs))?;
    // in the real world you are going to transmit or store this diff serialized to undiff format to be consumed or used somewhere elese
    let patch_txt = dmp.patch_to_text(&patches);

    Ok(patch_txt)
}

fn at_destination(patches: &str) -> Result<(), Error> {
    // initializing the module
    let dmp = DiffMatchPatch::new();
    // lets recreate the diffs from patches
    let patches = dmp.patch_from_text::<Efficient>(patches)?;
    // Now, lets apply these patches to the `old_txt` which is the original to get the new text
    let (new_txt, ops) = dmp.patch_apply(&patches, TXT_OLD)?;
    // Lets print out if the ops succeeded or not
    ops.iter()
        .for_each(|&o| println!("{}", if o { "OK" } else { "FAIL" }));

    // If everything goes as per plan you should see
    // OK
    // OK
    // ... and so on

    // lets check out if our `NEW_TXT` (presumably the edited one)
    if new_txt != TXT_NEW {
        return Err(Error::InvalidInput);
    }

    println!("Wallah! Patch applied successfully!");

    Ok(())
}

fn main() -> Result<(), Error> {
    // At the source of diff where the old text is being edited we'll create a set of patches
    let patches = at_source()?;
    // We'll send this diff to some destination e.g. db or the client where these changes are going to be applied
    // The destination will receive the patch string and will apply the patches to recreate the edits
    at_destination(&patches)
}

```

### `Compat` mode

```rust
use diff_match_patch_rs::{DiffMatchPatch, Compat, Error, PatchInput};

// This is the source text
const TXT_OLD: &str = "I am the very model of a modern Major-General, I've information on vegetable, animal, and mineral, 🚀👏👀";

// Let's assume this to be the text that was editted from the source text
const TXT_NEW: &str = "I am the very model of a cartoon individual, My animation's comical, unusual, and whimsical.😊👀";

// An example of a function that creates a diff and returns a set of patches serialized
fn at_source() -> Result<String, Error> {
    // initializing the module
    let dmp = DiffMatchPatch::new();
    // create a list of diffs
    let diffs = dmp.diff_main::<Compat>(TXT_OLD, TXT_NEW)?;
    // Now, we are going to create a list of `patches` to be applied to the old text to get the new text
    let patches = dmp.patch_make(PatchInput::new_diffs(&diffs))?;
    // in the real world you are going to transmit or store this diff serialized to undiff format to be consumed or used somewhere elese
    let patch_txt = dmp.patch_to_text(&patches);

    Ok(patch_txt)
}

fn at_destination(patches: &str) -> Result<(), Error> {
    // initializing the module
    let dmp = DiffMatchPatch::new();
    // lets recreate the diffs from patches
    let patches = dmp.patch_from_text::<Compat>(patches)?;
    // Now, lets apply these patches to the `old_txt` which is the original to get the new text
    let (new_txt, ops) = dmp.patch_apply(&patches, TXT_OLD)?;
    // Lets print out if the ops succeeded or not
    ops.iter()
        .for_each(|&o| println!("{}", if o { "OK" } else { "FAIL" }));

    // If everything goes as per plan you should see
    // OK
    // OK
    // ... and so on

    // lets check out if our `NEW_TXT` (presumably the edited one)
    if new_txt != TXT_NEW {
        return Err(Error::InvalidInput);
    }

    println!("Wallah! Patch applied successfully!");

    Ok(())
}

fn main() -> Result<(), Error> {
    // At the source of diff where the old text is being edited we'll create a set of patches
    let patches = at_source()?;
    // We'll send this diff to some destination e.g. db or the client where these changes are going to be applied
    // The destination will receive the patch string and will apply the patches to recreate the edits
    at_destination(&patches)
}
```

#### Note
The `Efficient` and `Compat` mode APIs are identical with the only chage being the `generic` parameter declared during the calls.

E.g. we initiated a `diff` in the `Efficient` mode with `dmp.diff_main::<Efficient>( ... )` while for `Compat` mode we did `dmp.diff_main::<Compat>( ... )`.

Please checkout the `examples` directory of the [source repo](https://github.com/AnubhabB/diff-match-patch-rs/tree/main/examples) for a few common use-cases.

<div class="warning">The `Effitient` and `Compat` modes are mutually exclusive and will not generate correct output if used interchangibly at source and destination</div>

## Benchmarks
Benchmarks are maintained [diff-match-patch-bench repository](https://github.com/AnubhabB/diff-match-patch-rs-bench)

| Lang.   | Library                                                                                  | Diff Avg. | Patch Avg. | Bencher    | Mode        | Correct |
|:-------:|:----------------------------------------------------------------------------------------:|:---------:|:----------:|:----------:|:-----------:|:-------:|
| `rust`  | [diff_match_patch v0.1.1](https://crates.io/crates/diff_match_patch)[^2]                 | 68.108 ms | 10.596 ms | Criterion   | -           |    ✅   |
| `rust`  | [diffmatchpatch v0.0.4](https://crates.io/crates/diffmatchpatch)[^3]                     | 66.454 ms | -         | Criterion   | -           |    ❌   |
| `rust`  | [dmp v0.2.0](https://crates.io/crates/dmp)                                               | 69.019 ms | 14.654 ms | Criterion   | -           |    ✅   |
| `rust`  | [diff-match-patch-rs](https://github.com/AnubhabB/diff-match-patch-rs.git)<sup>our</sup> | 65.487 ms | 631.13 µs | Criterion   | `Efficient` |    ✅   |
| `rust`  | [diff-match-patch-rs](https://github.com/AnubhabB/diff-match-patch-rs.git)<sup>our</sup> | 65.642 ms | 1.1703 ms | Criterion   | `Compat`    |    ✅   |
| `go`    | [go-diff](https://github.com/sergi/go-diff)[^1]                                  | 50.31 ms  | 135.2 ms  | go test     | -           |    ❌   |
| `node`  | [diff-match-patch](https://www.npmjs.com/package/diff-match-patch)                       | 246.90 ms | 1.07 ms   | tinybench   | -           |    ✅   |
| `python`| [diff-match-patch](https://pypi.org/project/diff-match-patch/)                           | 1.01 s    | 0.25 ms   | timeit      | -           |    ✅   |


[^1]: [go-diff](https://github.com/sergi/go-diff) seems to generate wrong diffs for emoticons. This benchmark is on the text with the emoticons removed.
[^2]: Adds an extra clone to the iterator because the `patch_apply` method takes mutable refc. to `diffs`.
[^3]: The crate [diffmatchpatch v0.0.4](https://crates.io/crates/diffmatchpatch) is still a WIP, cound't find the `patch_apply` method.



## Related projects

Diff Match Patch was originally built in 2006 to power Google Docs.
- [Diff Match Patch](https://github.com/google/diff-match-patch) (and it's [fork](https://github.com/dmsnell/diff-match-patch))
- **Rust**: [Distil.io diff_match_patch](https://crates.io/crates/diff_match_patch)
- **Rust**: [dmp](https://crates.io/crates/dmp)
- **Rust**: [Dissimilar](https://crates.io/crates/dissimilar) by the awesome [David Tolnay](https://github.com/dtolnay)
- **Rust**: [diff_match_patch](https://crates.io/crates/diff_match_patch)



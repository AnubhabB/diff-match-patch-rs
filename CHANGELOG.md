# CHANGELOG.md

## 0.4.1
Minor performance & cleanup to Diff:

    - the techniques to attain performance gains in `v0.4.0` now applied across the entire diff flow.
    - some cleanups and simplification

## 0.4.0
Performance:

    - Non-algorithmic improvements to `Diff` implementation resulting in `~41%` improvements over previous benchmarks. Experiments: https://blog.anubhab.me/tech/optimizing-diff-match-patch/

## 0.3.2
Fix:

    - Minor fix for [Issue](https://github.com/AnubhabB/diff-match-patch-rs/issues/7)

## 0.3.1
Fix:

    - Fixing order of Ops definition [Issue](https://github.com/AnubhabB/diff-match-patch-rs/issues/5)

## 0.3.0
Breaking Change:

    - the `match_main` API now supports `Efficient` and `Compat` modes. The call to `match_main` is now `match_main::<Efficient>` or `match_main::<Compat>` depending on your use-case

## 0.2.1
Fix:

    - fixed bug in optional dependency `chrono` based on target `wasm32-unknown-unknown`

## 0.2.0
Features:

    - stabilizing APIs & coming out of beta
    - removes dependency burden on `chrono` for non-wasm targets - minor performance improvements for non-wasm targets
    - tested and added more targets

Fix:

    - Fixes a panic [Issue](https://github.com/AnubhabB/diff-match-patch-rs/issues/2)

General:

    - elaborate compatibility tests with python, go and js libs. [Here](https://github.com/AnubhabB/diff-match-patch-rs-bench)

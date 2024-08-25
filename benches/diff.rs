use std::path::Path;

use criterion::{criterion_group, criterion_main, Criterion};
use diff_match_patch_rs::dmp::DiffMatchPatch;

fn diff_main(c: &mut Criterion) {
    let basedir = Path::new("testdata");
    let old = std::fs::read_to_string(basedir.join("txt_old.txt")).unwrap();
    let new = std::fs::read_to_string(basedir.join("txt_new.txt")).unwrap();

    let dmp = DiffMatchPatch::default();

    c.bench_function("diff-match-patch", |bencher| {
        bencher.iter(|| dmp.diff_main_compat(&old, &new).unwrap());
    });
}

criterion_group!(diff, diff_main);
criterion_main!(diff);

use std::path::Path;

use criterion::{criterion_group, criterion_main, Criterion};
use diff_match_patch_rs::dmp::DiffMatchPatch;

fn diff_main(c: &mut Criterion) {
    let basedir = Path::new("testdata");
    let old = std::fs::read_to_string(basedir.join("txt_old.txt")).unwrap();
    let new = std::fs::read_to_string(basedir.join("txt_new.txt")).unwrap();

    let dmp = DiffMatchPatch::default();

    let oldchars = old.chars().collect::<Vec<_>>();
    let newchars = new.chars().collect::<Vec<_>>();

    c.bench_function("diff-match-patch-chars", |bencher| {
        bencher.iter(|| dmp.diff_main(&oldchars, &newchars).unwrap());
    });

    c.bench_function("diff-match-patch-bytes", |bencher| {
        bencher.iter(|| dmp.diff_main(old.as_bytes(), new.as_bytes()).unwrap());
    });
}

criterion_group!(diff, diff_main);
criterion_main!(diff);

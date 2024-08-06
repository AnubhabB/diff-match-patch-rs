use core::panic;
use std::path::Path;

use criterion::{criterion_group, criterion_main, Criterion};

fn prefix_linear(a: &str, b: &str, res: usize) {
    let found = a.bytes().zip(b.bytes()).take_while(|(a, b)| a == b).count();

    if found != res {
        panic!()
    }
}

fn prefix_binary(a: &str, b: &str, res: usize) {
    let a = a.as_bytes();
    let b = b.as_bytes();

    let mut pointmin = 0;
    let mut pointmax = a.len().min(b.len());
    let mut pointmid = pointmax;

    let mut pointstart = 0;

    while pointmin < pointmid {
        if a[pointstart..pointmid] == b[pointstart..pointmid] {
            pointmin = pointmid;
            pointstart = pointmin;
        } else {
            pointmax = pointmid;
        }

        pointmid = (pointmax - pointmin) / 2 + pointmin;
    }

    if pointmid != res {
        panic!("Not desired res")
    }
}

fn create_data() -> Vec<(String, String, usize, usize)> {
    let basedir = Path::new("testdata");
    let data = [100, 1000, 10000, 100000, 1000000, 10000000]
        .iter()
        .map(|&n| {
            let old = std::fs::read_to_string(basedir.join(format!("old_{n}.txt"))).unwrap();
            let new = std::fs::read_to_string(basedir.join(format!("new_{n}.txt"))).unwrap();

            let res = std::fs::read_to_string(basedir.join(format!("ans_{n}.txt")))
                .unwrap()
                .parse::<usize>()
                .unwrap();

            (old, new, res, n)
        })
        .collect();

    data
}

pub fn prefix_bench(c: &mut Criterion) {
    let d = create_data();

    for (old, new, res, n) in d.iter() {
        println!("For N={n}");
        c.bench_function("prefix linear", |bencher| {
            bencher.iter(|| prefix_linear(old, new, *res))
        });
        c.bench_function("prefix binary", |bencher| {
            bencher.iter(|| prefix_binary(old, new, *res))
        });
    }
}

criterion_group!(prefix, prefix_bench);
criterion_main!(prefix);

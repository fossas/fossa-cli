use std::path::{Path, PathBuf};

use berkeleydb::BerkeleyDB;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

#[global_allocator]
static ALLOC: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

fn parse_single(path: &Path) {
    let target = path.join("Packages");
    let mut db = BerkeleyDB::open(&target).expect("must open path");

    for value in db.read().expect("must parse database") {
        black_box(value);
    }
}

fn bench_parse(c: &mut Criterion, path: impl Into<PathBuf>) {
    let path = path.into();
    let name = format!("parse '{}'", path.display());
    c.bench_function(&name, |b| b.iter(|| parse_single(&path)));
}

fn criterion_benchmark(c: &mut Criterion) {
    bench_parse(c, "./testdata/centos5-plain");
    bench_parse(c, "./testdata/centos6-devtools");
    bench_parse(c, "./testdata/centos6-many");
    bench_parse(c, "./testdata/centos6-plain");
    bench_parse(c, "./testdata/centos7-devtools");
    bench_parse(c, "./testdata/centos7-httpd24");
    bench_parse(c, "./testdata/centos7-many");
    bench_parse(c, "./testdata/centos7-plain");
    bench_parse(c, "./testdata/centos7-python35");
    bench_parse(c, "./testdata/centos8-modularitylabel");
    bench_parse(c, "./testdata/ubi8-s390x");
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

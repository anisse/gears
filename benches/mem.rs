use criterion::{black_box, criterion_group, criterion_main, Criterion};
use gears::mem;

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut mem = mem::Memory::init(64 * 1024); // This test suite is for machines with more RAM
    c.bench_function("mem set", |b| b.iter(|| mem.set_u8(black_box(0), 42)));
}

pub fn criterion_benchmark_long(c: &mut Criterion) {
    let mut mem = mem::Memory::init(64 * 1024); // This test suite is for machines with more RAM
    c.bench_function("mem set full", |b| {
        b.iter(|| {
            for i in 0..65535 {
                mem.set_u8(black_box(i), 42);
            }
        })
    });
}

criterion_group!(benches, criterion_benchmark, criterion_benchmark_long);
criterion_main!(benches);

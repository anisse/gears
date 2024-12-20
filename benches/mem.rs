use criterion::{black_box, criterion_group, criterion_main, Criterion};
use gears::mem;
use gears::mem::MemoryMapper;

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut mem = mem::ZX64kMapper::default(); // This test suite is for machines with more RAM
    c.bench_function("mem set", |b| b.iter(|| mem.set_u8(black_box(0), 42)));
}

pub fn criterion_benchmark_long(c: &mut Criterion) {
    let mut mem = mem::ZX64kMapper::default(); // This test suite is for machines with more RAM
    c.bench_function("mem set full", |b| {
        b.iter(|| {
            for i in 0..65535 {
                mem.set_u8(black_box(i), 42);
            }
        })
    });
}

pub fn criterion_benchmark_gg(c: &mut Criterion) {
    let mut mem = mem::SegaGGMapper::new(vec![0; 32762], None); // This test suite is for machines with more RAM
    c.bench_function("mem set gg", |b| {
        b.iter(|| {
            for i in 0xC000..0xFFFC {
                mem.set_u8(black_box(i), 42);
            }
        })
    });
}

criterion_group!(
    benches,
    criterion_benchmark,
    criterion_benchmark_long,
    criterion_benchmark_gg
);
criterion_main!(benches);

/// Static builds on Linux suffer immensely with the default libc allocator,
/// but jemalloc suffers a lot less (~30% performance hit, compared to ~600%).
///
/// Through benchmarking, jemalloc appears to be roughly equivalent to libc,
/// so rather than deal with different feature flags for different systems
/// we'll just enable it unconditionally.
///
/// Reference: https://github.com/fossas/broker/blob/main/docs/dev/reference/static-binary.md
#[global_allocator]
static ALLOC: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

fn main() {
    println!("Hello, world!");
}

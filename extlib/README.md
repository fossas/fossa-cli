
# about

`extlib` is the home for libraries or library-like binaries that are used in FOSSA CLI but written in a different language (namely, Rust).

# build artifacts

Build libraries with `cargo build` (optionally, use `--release` for release-optimized builds).
From there, the output is found in `target/{release|debug}/{crate_name}`.

Depending on the target:
- If it is a binary crate, it should have no extension or a `.exe` for Windows.
- If it is a library, it should have the appropriate extension for the link type and platform.

# static builds

Static builds on Linux suffer immensely with the default libc allocator,
but jemalloc suffers a lot less (~30% performance hit, compared to ~600%).

Through benchmarking jemalloc appears to be roughly equivalently performant to libc
outside of static linking contexts, so rather than deal with different feature flags
for different systems the binaries in this repo just use it unconditionally.

Reference: https://github.com/fossas/broker/blob/main/docs/dev/reference/static-binary.md

# cross compilation

Rust programs can (usually) easily be built for other platforms.

Any pure Rust library should be fine to cross compile,
but if the program makes use of system libraries it is usually better to
build directly on the target platform because there's less ambiguity
about how well those libraries tolerate cross compilation.

## cross architecture

Install additional targets and just build to those targets:

```shell
rustup target add aarch64-apple-darwin
rustup target add x86_64-apple-darwin

# Places assets in target/aarch64-apple-darwin/
cargo build --target aarch64-apple-darwin

# Places assets in target/x86_64-apple-darwin/
cargo build --target x86_64-apple-darwin
```

## cross platform

For this example we'll build targeting Windows from a non-Windows environment.
The easiest way is to use `cross` to build inside Docker:

1. Install [`cross`](https://github.com/cross-rs/cross): `cargo binstall cross`
1. `cross build --target x86_64-pc-windows-gnu`

This can also be done natively, similar to what is done for cross architecture.
Note that this may take additional work depending on the target platform
and system libraries being used:

1. Install the `mingw-64` package.
  - On macOS, that's `brew install mingw-w64`
  - On Debian/Ubuntu, that's `sudo apt-get install mingw-w64`
  - On Arch, that's `sudo pacman -S mingw-w64`, install all
1. `rustup target add x86_64-pc-windows-gnu`
1. `cargo build --target x86_64-pc-windows-gnu`

## cross version

You can also do this across Rust versions:

```shell
rustup install 1.68

# Run a normal cargo build
rustup run 1.68 cargo build

# Run any cross build
rustup run 1.68 cross build --target x86_64-pc-windows-gnu
```

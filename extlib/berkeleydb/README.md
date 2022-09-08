
# berkeleydb

Heavily based on [this Go module](https://github.com/knqyf263/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/bdb.go).

It was built because there are a ton of imperative file handle seeks and other design issues
that made it really awkward to build and maintain in Haskell.

## output

UTF-8 JSON formatted output, in the shape `Array<String>`.
Array entries are Base64 encoded strings meant to be decoded into `ByteString` and then parsed with `Data.Rpm.DbHeaderBlob`.

## usage

This library may be consumed as a Rust library or an C FFI library.

1. Link against the FFI library.
2. Call functions.
3. Parse output.

The intention is that callers:

1. Read the content of the file into memory.
2. Pass a pointer to that content into `parse_buf`.

### as a Rust library

Reference via path dependency using proper types as usual.

## building

To build the library, run `cargo build`.
If release mode is desired, append `--release`.
Output symbols are found at `target/debug/libbdb.a` or `target/release/libbdb.a`.

## linking

This library is built statically (`.a`).
As an example to demonstrate static linking, create the following C file:

```c
// call_rust.c

extern void hello_from_rust();

int main(void) {
    hello_from_rust();
    return 0;
}
```

Then build:
```shell
gcc call_rust.c -o call_rust -lbdb -L../../target/debug
```

Observe that the result works and is statically linked with the Rust binary (on Linux, use `ldd` instead of `otool`):
```shell
; gcc call_rust.c -o call_rust -lbdb -L../../target/debug
; otool -L call_rust
call_rust:
	/usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1311.100.3)
; ./call_rust
Hello from Rust!
```

Translating this to Haskell, we should just need to provide the same sort of C flags:
```

```

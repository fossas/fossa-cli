
# berkeleydb

Heavily based on [this Go module](https://github.com/knqyf263/go-rpmdb/blob/956701287363101ee9ade742d6bf1d5c5495f62a/pkg/bdb/bdb.go).

This is intended to be a thin CLI wrapper that parses the BDB container format.

It was built because there are a ton of imperative file handle seeks and other design issues
that made it really awkward to build and maintain in Haskell.

## usage

```shell
; berkeleydb {file}
# JSON formatted output
```

## output format

TODO

## building

In the root directory, run `cargo build --bin berkeleydb`.
If release mode is desired, append `--release`.

Output symbols are found in `target/debug/berkeleydb` or `target/release/berkeleydb`.

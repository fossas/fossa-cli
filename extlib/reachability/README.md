# reachability

Parses source code, and produces call graphs for
set of source code files.

## usage

```shell
; reachability --target {directory}
; reachability --target {path_to_file}

# with stdin
; cat {file} | reachability 
```

## building

In the root directory, run `cargo build --bin reachability`.
If release mode is desired, append `--release`.

Output symbols are found in `target/debug/reachability` or `target/release/reachability`.

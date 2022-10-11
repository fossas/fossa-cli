# Erlang Analysis

When developing in Erlang, [Rebar](https://www.rebar3.org/) is the most common package manager. Dependencies are specified in a manifest file by users which is used by the `rebar3` tool to build a dependency graph and download the correct dependencies.


| Strategy | Direct Deps        | Transitive Deps          | Edges              | Container Scanning (experimental) |
| -------- | ------------------ | ------------------ | ------------------ |
| rebar3   | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                               |

## Project Discovery

In order to find Rebar projects, we look for `rebar.config` files which specify the root of a Rebar project. Once we find a `rebar.config` file we quit walking the file tree because the downloaded dependencies will have their own `rebar.config` files in subdirectories. 

## Analysis

1. run `rebar3 tree -v` and generate output similar to:
```
├─ one─1.0.0 (https://github.com/dep/one)
│  ├─ two─2.0.0 (hex package)
│  │  └─ three─3.0.0 (hex package)
│  └─ four─4.0.0 (https://github.com/dep/four)
└─ five─5.0.0 (hex package)
```
2. Parse this output to determine the dependency graph and the locations of each dependency.

## Limitations

#### Dependency name aliasing
`rebar.config` allows users to alias dependencies in a format similar to `{deps, [{chatterbox, {pkg, ts_chatterbox}},`. This format is not currently handled in the CLI and will result in unknown dependencies being shown in the FOSSA UI. The team is aware of this limitation and a fix is planned.

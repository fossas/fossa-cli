# CLAUDE.md - Guidelines for Fossa CLI

## Build Commands
- Haskell: `cabal build`, `make build-cli`
- Rust: `cargo build --release`
- Full Build: `make build` (builds both)
- Unit Tests (Haskell): `cabal test unit-tests` or `make test-cabal ARGS="TestName"`
- Unit Tests (Rust): `cargo test` or `make test-cargo`
- Integration Tests: `make integration-test ARGS="TestName"`
- Format Code: `make fmt` (runs both formatters)
- Lint Code: `make lint` (runs both linters)
- Analyze Project: `make analyze`

## Code Quality Standards
- Compiler warnings should be treated as errors and fixed, not ignored
- Linter warnings should be treated as errors and fixed, not ignored
- All tests must pass before submitting code
- Code should be thoroughly tested with appropriate unit tests

## Code Style Guidelines - Haskell
- GHC Version: 9.8
- Formatting: `fourmolu` with 2 spaces, leading commas
- Imports: Use explicit imports, qualified with full names
- Types: Prefer `newtype`, use `Text` instead of `String`
- Functions: Avoid partial functions, list comprehensions, match guards
- Error handling: Never use `error` or `undefined`

## Code Style Guidelines - Rust
- Format with `rustfmt` via `cargo fmt`
- Lint with Clippy via `cargo clippy`
- Follow standard Rust idioms including error handling
- Use the Rust embedded tools within the Haskell code via the extlib directory

For complete guidelines, see `docs/contributing/STYLE-GUIDE.md`
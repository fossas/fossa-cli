#!/usr/bin/env sh

set -e
set -x

if [ $# -lt 2 ] ; then
    echo "Usage: ./build-arm.sh <RUNNER OS> <PROJECT FILE>"
    exit 1
fi

RUNNER_OS=$1
PROJECT_FILE=$2

# Install rust tooling
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --profile minimal
cargo install nextest

rustc -V
cargo -V

# Build Rust binaries
cargo build --release

cargo nextest run --release

# Now do Haskell things
./build.sh "$RUNNER_OS" "$PROJECT_FILE"

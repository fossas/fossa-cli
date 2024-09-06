#!/usr/bin/env sh

set -e
set -x

if [ $# -lt 2 ] ; then
    echo "Usage: ./build-arm.sh <RUNNER OS> <PROJECT FILE> [FEATURES]"
    exit 1
fi

RUNNER_OS=$1
PROJECT_FILE=$2
FEATURES=$3

# Install rust tooling
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --profile minimal
. "/home/runner/.cargo/env"

rustc -V
cargo -V

# Build Rust binaries
cargo build ${FEATURES:+--features $FEATURES} jemalloc --release

cargo test --release

# Validate that diagnose runs
cargo run --release --bin diagnose -- walk --trace-spans none --trace-level info

# Now do Haskell things
./.github/workflows/scripts/build.sh "$RUNNER_OS" "$PROJECT_FILE"

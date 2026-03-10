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

# Use workspace-relative paths so the GitHub Actions cache (which runs on the
# host) can restore/save the Rust toolchain and build artifacts. Without this,
# rustup installs to /home/runner/.cargo inside the container, which the host
# cache action never sees — causing a full reinstall on every run.
export CARGO_HOME="${GITHUB_WORKSPACE}/.rust/cargo"
export RUSTUP_HOME="${GITHUB_WORKSPACE}/.rust/rustup"
export PATH="${CARGO_HOME}/bin:${PATH}"

# Install Rust only if not restored from cache.
if ! command -v cargo > /dev/null 2>&1; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --profile minimal
fi

rustc -V
cargo -V

# Build Rust binaries
cargo build ${FEATURES:+--features $FEATURES} --release

cargo test --release

# Validate that diagnose runs
cargo run --release --bin diagnose -- walk --trace-spans none --trace-level info

# Now do Haskell things
./.github/workflows/scripts/build.sh "$RUNNER_OS" "$PROJECT_FILE"

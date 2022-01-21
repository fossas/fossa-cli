#!/usr/bin/env bash

# exit when any command fails
set -e

# this script assumes it's running from the root of the project (e.g. via `make`).
# The binaries are checked in to git so that everyone doesn't have to run this script,
# but I wanted to include the build artifacts anyway.

ROOT=test/App/Fossa/VSI/DynLinked/testdata

gcc $ROOT/hello.c -o $ROOT/hello_standard
cp $ROOT/hello_standard $ROOT/hello_setuid

chmod a+s $ROOT/hello_setuid

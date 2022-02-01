#!/usr/bin/env bash

# exit when any command fails
set -e

# this script assumes it's running from the root of the project (e.g. via `make`).

ROOT=test/App/Fossa/VSI/DynLinked/testdata

if [ -f "$ROOT/hello_standard" ]; then
  rm $ROOT/hello_standard
fi

if [ -f "$ROOT/hello_setuid" ]; then
  rm $ROOT/hello_setuid
fi

#!/usr/bin/env sh

set -e

make build-test-data
cabal update
cabal build --project-file=cabal.project.ci.linux all
cabal test --project-file=cabal.project.ci.linux

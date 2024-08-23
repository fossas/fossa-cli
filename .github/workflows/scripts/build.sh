#!/usr/bin/env sh

cabal update && cabal build --project-file=cabal.project.ci.linux

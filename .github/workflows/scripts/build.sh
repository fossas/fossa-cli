#!/usr/bin/env sh

set -e
set -x

pwd
ls -al

if [ $# -lt 2 ] ; then
    echo "Usage: ./build.sh <RUNNER OS> <PROJECT FILE>"
    exit 1
fi

RUNNER_OS=$1
PROJECT_FILE=$2

make build-test-data

# Does this need to run in the container?
# It normally runs outside.
git config --global --add safe.directory "$GITHUB_WORKSPACE"

# With dist-newstyle caches:
# Cabal mainly knows to recompile based on changes to files.
# Tagging in git doesn't reliably change a file in a fixed location that cabal/GHC can track to indicate that there's a new tag.
# For our release process, we merge to master, which builds (and may store a dist-newstyle cache), then push a release tag.
# During the tag build, cabal/GHC may not realize that they have to rebuild the Version.hs file because the tag is invisible to it.
# This line adds a comment to our version source file to prompt cabal/GHC to rebuild Version.hs.
echo "{- $GITHUB_RUN_ID -}" >> src/App/Version.hs
cabal update
cabal build --project-file="$PROJECT_FILE" all
cabal test --project-file="$PROJECT_FILE" unit-tests

# TODO: Bring this back.
# Test cabal-install.
# This check ensures that QuickImport can use spectrometer as a library.
# if [ "$RUNNER_OS" = 'Linux' ] ; then
#     cabal install --overwrite-policy=always --project-file="$PROJECT_FILE" --ghc-options="-Wwarn"
# fi

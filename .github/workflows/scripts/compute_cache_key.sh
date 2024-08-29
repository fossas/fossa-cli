#!/usr/bin/env sh

set -e
set -x

if [ $# -lt 2 ] ; then
    echo "Usage: ./compute_cache_key <RUNNER OS> <PROJECT FILE>"
    exit 1
fi

RUNNER_OS=$1
PROJECT_FILE=$2

cabal --project-file="$PROJECT_FILE" update
cabal --project-file="$PROJECT_FILE" build --dry-run
jq '."install-plan"[]."id"' < dist-newstyle/cache/plan.json | sort > /tmp/cabal-cache-key
echo "Install plan:"
cat /tmp/cabal-cache-key

if [ "$RUNNER_OS" = "macOS" ]; then
   PLAN_SUM=$(shasum -a256 /tmp/cabal-cache-key)
else
   PLAN_SUM=$(sha256sum /tmp/cabal-cache-key)
fi

CABAL_CACHE_KEY="$(echo "$PLAN_SUM" | awk '{print $1}')"
export CABAL_CACHE_KEY
echo "Cabal cache key: $CABAL_CACHE_KEY"
echo "cabal-cache-key=$CABAL_CACHE_KEY" >> "$GITHUB_OUTPUT"

echo "$HOME"

# Cleanup. Restoring this cache seems to fail if the directory already exists.
# TODO: See if we can just modify permissions and dump the old cache on top of this script's build products.
rm -rf dist-newstyle

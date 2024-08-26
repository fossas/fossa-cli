#!/usr/bin/env sh

RUNNER_OS=$1
PROJECT_FILE=$2

cabal --project-file="$PROJECT_FILE" update
cabal --project-file="$PROJECT_FILE" build --dry-run
cat dist-newstyle/cache/plan.json | jq '."install-plan"[]."id"' | sort > /tmp/cabal-cache-key
echo "Install plan:"
cat /tmp/cabal-cache-key

if [ "$RUNNER_OS" = "macOS" ]; then
   PLAN_SUM=$(shasum -a256 /tmp/cabal-cache-key)
else
   PLAN_SUM=$(sha256sum /tmp/cabal-cache-key)
fi

export CABAL_CACHE_KEY=$(echo $PLAN_SUM | awk '{print $1}')
echo "Cabal cache key: $CABAL_CACHE_KEY"
echo "cabal-cache-key=$CABAL_CACHE_KEY" >> $GITHUB_OUTPUT

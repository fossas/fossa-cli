#!/usr/bin/env bash
#
# Requires environment variables:
#   GITHUB_TOKEN    A token with access to the fossas/basis repository
#
# Requires binary dependencies in $PATH:
#   jq              Parse and manipulate json structures.
#   curl            Download data over HTTP(s)
#   upx             Compress binaries (optional)
#

set -e

if [ -z "$GITHUB_TOKEN" ]; then
  echo "Provide your GITHUB_TOKEN in the environment"
  exit 1
fi

rm -f vendor/*
mkdir -p vendor

ASSET_POSTFIX=""
case "$(uname -s)" in
# case "Linux" in
  Darwin)
    ASSET_POSTFIX="darwin"
    ;;

  Linux)
    ASSET_POSTFIX="linux"
    ;;
  
  *)
    echo "Supported hosts are MacOS and Linux"
    exit 1
    ;;
esac

TAG="latest"
echo "Downloading asset information from latest tag for architecture '$ASSET_POSTFIX'"

FILTER=".name == \"wiggins-$ASSET_POSTFIX\""
curl -sL -H "Authorization: token $GITHUB_TOKEN" -H "Accept: application/vnd.github.v3.raw" -s api.github.com/repos/fossas/basis/releases/latest | jq -c ".assets | map({url: .url, name: .name}) | map(select($FILTER)) | .[]" | while read ASSET; do
  URL="$(echo $ASSET | jq -c -r '.url')"
  NAME="$(echo $ASSET | jq -c -r '.name')"
  OUTPUT=vendor/${NAME%"-$ASSET_POSTFIX"}

  echo "Downloading '$NAME' to '$OUTPUT'"
  curl -sL -H "Authorization: token $GITHUB_TOKEN" -H "Accept: application/octet-stream" -s $URL > $OUTPUT
done

echo "Marking binaries executable"
chmod +x vendor/*

echo "Compressing binaries"
upx vendor/* || echo "WARN: 'upx' command not found, binaries will not be compressed"

echo "Vendored binaries are ready for use"
ls -lh vendor/

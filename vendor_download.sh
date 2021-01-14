#!/usr/bin/env bash
#
# Requires environment variables:
#   GITHUB_TOKEN    A token with access to the fossas/basis repository
#
# Requires binary dependencies in $PATH:
#   jq              Parse and manipulate json structures.
#   curl            Download data over HTTP(s)
#   sed             Modify syft tag
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


echo "Downloading wiggins binary"
WIGGINS_RELEASE_JSON=vendor/wiggins-release.json
curl -sSL \
    -H "Authorization: token $GITHUB_TOKEN" \
    -H "Accept: application/vnd.github.v3.raw" \
    api.github.com/repos/fossas/basis/releases/latest > $WIGGINS_RELEASE_JSON

WIGGINS_TAG=$(jq -cr ".name" $WIGGINS_RELEASE_JSON)
FILTER=".name == \"wiggins-$ASSET_POSTFIX\""
echo "Using wiggins release: $WIGGINS_TAG"
jq -c ".assets | map({url: .url, name: .name}) | map(select($FILTER)) | .[]" $WIGGINS_RELEASE_JSON | while read ASSET; do
  URL="$(echo $ASSET | jq -c -r '.url')"
  NAME="$(echo $ASSET | jq -c -r '.name')"
  OUTPUT=vendor/${NAME%"-$ASSET_POSTFIX"}

  echo "Downloading '$NAME' to '$OUTPUT'"
  curl -sL -H "Authorization: token $GITHUB_TOKEN" -H "Accept: application/octet-stream" -s $URL > $OUTPUT
done
rm $WIGGINS_RELEASE_JSON
echo "Wiggins download successful"
echo

echo "Downloading forked syft binary"
SYFT_RELEASE_JSON=vendor/syft-release.json
curl -sSL \
    -H "Authorization: token $GITHUB_TOKEN" \
    -H "Accept: application/vnd.github.v3.raw" \
    api.github.com/repos/fossas/syft/releases/latest > $SYFT_RELEASE_JSON

# Remove leading 'v' from version tag
# 'v123' -> '123'
SYFT_TAG=$(jq -cr '.name' $SYFT_RELEASE_JSON | sed 's/^v//')
echo "Using fossas/syft release: $SYFT_TAG"
FILTER=".name == \"container-scanning_${SYFT_TAG}_${ASSET_POSTFIX}_amd64.tar.gz\""
jq -c ".assets | map({url: .url, name: .name}) | map(select($FILTER)) | .[]" $SYFT_RELEASE_JSON | while read ASSET; do
  URL="$(echo $ASSET | jq -c -r '.url')"
  NAME="$(echo $ASSET | jq -c -r '.name')"
  OUTPUT=vendor/${NAME%"-$ASSET_POSTFIX"}

  echo "Downloading '$NAME' to '$OUTPUT'"
  curl -sL -H "Authorization: token $GITHUB_TOKEN" -H "Accept: application/octet-stream" -s $URL > $OUTPUT
  echo "Extracting syft binary from tarball"
  tar xzf $OUTPUT fossa-container-scanning
  mv fossa-container-scanning vendor/syft
  rm $OUTPUT

done
rm $SYFT_RELEASE_JSON
echo "Forked Syft download successful"

echo "Marking binaries executable"
chmod +x vendor/*

echo "Compressing binaries"
upx vendor/* || echo "WARN: 'upx' command not found, binaries will not be compressed"

echo "Vendored binaries are ready for use"
ls -lh vendor/

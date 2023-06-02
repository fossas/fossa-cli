#!/usr/bin/env bash
#
# Requires environment variables:
#   GITHUB_TOKEN    A token with access to the fossas/basis repository
#
# Requires binary dependencies in $PATH:
#   jq              Parse and manipulate json structures.
#   curl            Download data over HTTP(s)
#   sed             Modify syft tag
#   xz              compress the license index
#   upx             Compress binaries (optional)
#

set -e

if [ -z "$GITHUB_TOKEN" ]; then
  echo "Provide your GITHUB_TOKEN in the environment"
  exit 1
fi

echo "curl version"
echo "------------"
curl --version
echo ""

echo "jq version"
echo "----------"
jq --version
echo ""

rm -f vendor-bins/*
mkdir -p vendor-bins

ASSET_POSTFIX=""
BASIS_ASSET_POSTFIX=""
OS_WINDOWS=false
case "$(uname -s)" in
  Darwin)
    ASSET_POSTFIX="darwin"
    BASIS_ASSET_POSTFIX="darwin-amd64"
    ;;

  Linux)
    ASSET_POSTFIX="linux"
    BASIS_ASSET_POSTFIX="linux-amd64"
    ;;

  *)
    echo "Warn: Assuming $(uname -s) is Windows"
    ASSET_POSTFIX="windows.exe"
    BASIS_ASSET_POSTFIX="windows-amd64"
    OS_WINDOWS=true
    ;;
esac

TAG="latest"
echo "Downloading asset information from latest tag for architecture '$ASSET_POSTFIX'"

THEMIS_TAG="2023-04-25-95b18b6-1682456045"
echo "Downloading themis binary"
echo "Using themis release: $THEMIS_TAG"
THEMIS_RELEASE_JSON=vendor-bins/themis-release.json
curl -sSL \
    -H "Authorization: token $GITHUB_TOKEN" \
    -H "Accept: application/vnd.github.v3.raw" \
    https://api.github.com/repos/fossas/basis/releases/tags/$THEMIS_TAG > $THEMIS_RELEASE_JSON

THEMIS_TAG=$(jq -cr ".name" $THEMIS_RELEASE_JSON)
FILTER=".name == \"themis-cli-$BASIS_ASSET_POSTFIX\""
jq -c ".assets | map({url: .url, name: .name}) | map(select($FILTER)) | .[]" $THEMIS_RELEASE_JSON | while read ASSET; do
  URL="$(echo $ASSET | jq -c -r '.url')"
  NAME="$(echo $ASSET | jq -c -r '.name')"
  OUTPUT="$(echo vendor-bins/$NAME | sed 's/-'$BASIS_ASSET_POSTFIX'$//')"

  echo "Downloading '$NAME' to '$OUTPUT'"
  curl -sL -H "Authorization: token $GITHUB_TOKEN" -H "Accept: application/octet-stream" -s $URL > $OUTPUT
done
echo "Themis download successful"

FILTER=".name == \"index.gob\""
jq -c ".assets | map({url: .url, name: .name}) | map(select($FILTER)) | .[]" $THEMIS_RELEASE_JSON | while read ASSET; do
  URL="$(echo $ASSET | jq -c -r '.url')"
  NAME="$(echo $ASSET | jq -c -r '.name')"
  OUTPUT="vendor-bins/$NAME"

  echo "Downloading '$NAME' to '$OUTPUT'"
  curl -sL -H "Authorization: token $GITHUB_TOKEN" -H "Accept: application/octet-stream" -s $URL > $OUTPUT
done
echo "themis index downloaded"

rm $THEMIS_RELEASE_JSON
echo

echo "Marking binaries executable"
chmod +x vendor-bins/*

echo "Compressing index.gob"
xz vendor-bins/index.gob

echo "Vendored binaries are ready for use"
ls -lh vendor-bins/

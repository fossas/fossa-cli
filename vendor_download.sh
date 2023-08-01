#!/usr/bin/env bash
#
# Requires environment variables:
#   GITHUB_TOKEN    A token with access to the fossas/basis repository
#
# Requires binary dependencies in $PATH:
#   jq              Parse and manipulate json structures.
#   curl            Download data over HTTP(s)
#   sed             Modify executable names
#   xz              compress the license index
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
LERNIE_ASSET_POSTFIX=""
OS_WINDOWS=false
case "$(uname -s)" in
  Darwin)
    ASSET_POSTFIX="darwin"
    BASIS_ASSET_POSTFIX="darwin-amd64"
    case "$(uname -m)" in
      arm64)
        LERNIE_ASSET_POSTFIX="aarch64-macos"
        ;;

      *)
        LERNIE_ASSET_POSTFIX="x86_64-macos"
        ;;
    esac
    ;;

  Linux)
    ASSET_POSTFIX="linux"
    BASIS_ASSET_POSTFIX="linux-amd64"
    LERNIE_ASSET_POSTFIX="linux"
    ;;

  *)
    echo "Warn: Assuming $(uname -s) is Windows"
    ASSET_POSTFIX="windows.exe"
    BASIS_ASSET_POSTFIX="windows-amd64"
    LERNIE_ASSET_POSTFIX="windows"
    OS_WINDOWS=true
    ;;
esac

# Download latest release of Themis and its index

TAG="latest"
echo "Downloading asset information from latest tag for architecture '$ASSET_POSTFIX'"

echo "Downloading themis binary from latest release"
THEMIS_RELEASE_JSON=vendor-bins/themis-release.json
curl -sSL \
    -H "Authorization: token $GITHUB_TOKEN" \
    -H "Accept: application/vnd.github.v3.raw" \
    https://api.github.com/repos/fossas/basis/releases/latest > $THEMIS_RELEASE_JSON

THEMIS_TAG=$(jq -cr ".name" $THEMIS_RELEASE_JSON)
echo "Using themis release: $THEMIS_TAG"
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

# Download latest release of Lernie

echo "Downloading lernie binary from latest release"
LERNIE_RELEASE_JSON=vendor-bins/lernie-release.json
curl -sSL \
    -H "Authorization: token $GITHUB_TOKEN" \
    -H "Accept: application/vnd.github.v3.raw" \
    https://api.github.com/repos/fossas/lernie/releases/latest > $LERNIE_RELEASE_JSON

LERNIE_TAG=$(jq -cr ".name" $LERNIE_RELEASE_JSON)
# Strip the leading 'v' off of the tag
LERNIE_VERSION=$(echo $LERNIE_TAG | sed -e 's/^v//')
FILTER=".name == \"lernie-$LERNIE_VERSION-$LERNIE_ASSET_POSTFIX\""
jq -c ".assets | map({url: .url, name: .name}) | map(select($FILTER)) | .[]" $LERNIE_RELEASE_JSON | while read ASSET; do
  URL="$(echo $ASSET | jq -c -r '.url')"
  NAME="$(echo $ASSET | jq -c -r '.name')"
  OUTPUT="$(echo vendor-bins/$NAME | sed 's/-'$LERNIE_VERSION'-'$LERNIE_ASSET_POSTFIX'$//')"

  echo "Downloading '$NAME' to '$OUTPUT'"
  curl -sL -H "Authorization: token $GITHUB_TOKEN" -H "Accept: application/octet-stream" -s $URL > $OUTPUT
done
echo "Lernie download successful"

rm $LERNIE_RELEASE_JSON

# Finished downloading

echo
echo "Marking binaries executable"
chmod +x vendor-bins/*

echo "Compressing index.gob"
xz vendor-bins/index.gob

echo "Vendored binaries are ready for use"
ls -lh vendor-bins/

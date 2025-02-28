#!/usr/bin/env bash
#
# Requires environment variables:
#   GITHUB_TOKEN    A token with access to the fossas/themis repository
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
THEMIS_ASSET_POSTFIX=""
LERNIE_ASSET_POSTFIX=""
CIRCE_ASSET_POSTFIX=""
case "$(uname -s)" in
  Darwin)
    case "$(uname -m)" in
      arm64)
        ASSET_POSTFIX="darwin-arm64"
        LERNIE_ASSET_POSTFIX="aarch64-macos"
        THEMIS_ASSET_POSTFIX="darwin-arm64"
        CIRCE_ASSET_POSTFIX="aarch64-apple-darwin"
        ;;

      *)
        ASSET_POSTFIX="darwin-amd64"
        LERNIE_ASSET_POSTFIX="x86_64-macos"
        THEMIS_ASSET_POSTFIX="darwin-amd64"
        CIRCE_ASSET_POSTFIX="x86_64-apple-darwin"
        ;;
    esac
    ;;

  Linux)
    case "$(uname -m)" in
      aarch64)
        ASSET_POSTFIX="linux"
        THEMIS_ASSET_POSTFIX="linux-arm64"
        LERNIE_ASSET_POSTFIX="aarch64-linux"
        CIRCE_ASSET_POSTFIX="aarch64-unknown-linux-gnu"
      ;;

      *)
        ASSET_POSTFIX="linux"
        THEMIS_ASSET_POSTFIX="linux-amd64"
        LERNIE_ASSET_POSTFIX="x86_64-linux"
        CIRCE_ASSET_POSTFIX="x86_64-unknown-linux-musl"
        ;;
    esac
    ;;
  *)
    echo "Warn: Assuming $(uname -s) is Windows"
    ASSET_POSTFIX="windows.exe"
    THEMIS_ASSET_POSTFIX="windows-amd64"
    LERNIE_ASSET_POSTFIX="x86_64-windows.exe"
    CIRCE_ASSET_POSTFIX="x86_64-pc-windows-msvc"
    ;;
esac

# Download latest release of Themis and its index

echo "Downloading asset information from latest tag for architecture '$ASSET_POSTFIX'"

echo "Downloading themis binary from latest release"
THEMIS_RELEASE_JSON=vendor-bins/themis-release.json
curl -sSL \
    -H "Authorization: token $GITHUB_TOKEN" \
    -H "Accept: application/vnd.github.v3.raw" \
    https://api.github.com/repos/fossas/themis/releases/latest > $THEMIS_RELEASE_JSON

THEMIS_TAG=$(jq -cr ".name" $THEMIS_RELEASE_JSON)
echo "Using themis release: $THEMIS_TAG"
FILTER=".name == \"themis-cli-$THEMIS_ASSET_POSTFIX\""
jq -c ".assets | map({url: .url, name: .name}) | map(select($FILTER)) | .[]" $THEMIS_RELEASE_JSON | while read -r ASSET; do
  URL="$(echo "$ASSET" | jq -c -r '.url')"
  NAME="$(echo "$ASSET" | jq -c -r '.name')"
  OUTPUT="$(echo vendor-bins/"$NAME" | sed 's/-'$THEMIS_ASSET_POSTFIX'$//')"

  echo "Downloading '$NAME' to '$OUTPUT'"
  curl -sL -H "Authorization: token $GITHUB_TOKEN" -H "Accept: application/octet-stream" -s "$URL" > "$OUTPUT"
done
echo "Themis download successful"

FILTER=".name == \"index.gob\""
jq -c ".assets | map({url: .url, name: .name}) | map(select($FILTER)) | .[]" $THEMIS_RELEASE_JSON | while read -r ASSET; do
  URL="$(echo "$ASSET" | jq -c -r '.url')"
  NAME="$(echo "$ASSET" | jq -c -r '.name')"
  OUTPUT="vendor-bins/$NAME"

  echo "Downloading '$NAME' to '$OUTPUT'"
  curl -sL -H "Authorization: token $GITHUB_TOKEN" -H "Accept: application/octet-stream" -s "$URL" > "$OUTPUT"
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
LERNIE_VERSION="${LERNIE_TAG/#v/}"
FILTER=".name == \"lernie-$LERNIE_VERSION-$LERNIE_ASSET_POSTFIX\""
jq -c ".assets | map({url: .url, name: .name}) | map(select($FILTER)) | .[]" $LERNIE_RELEASE_JSON | while read -r ASSET; do
  URL="$(echo "$ASSET" | jq -c -r '.url')"
  NAME="$(echo "$ASSET" | jq -c -r '.name')"
  OUTPUT="$(echo vendor-bins/"$NAME" | sed 's/-'"$LERNIE_VERSION"'-'$LERNIE_ASSET_POSTFIX'$//')"

  echo "Downloading '$NAME' to '$OUTPUT'"
  curl -sL -H "Authorization: token $GITHUB_TOKEN" -H "Accept: application/octet-stream" -s "$URL" > "$OUTPUT"
done
echo "Lernie download successful"

rm $LERNIE_RELEASE_JSON

# Download latest release of Circe

echo "Downloading circe binary from latest release"
CIRCE_RELEASE_JSON=vendor-bins/circe-release.json
curl -sSL \
    -H "Authorization: token $GITHUB_TOKEN" \
    -H "Accept: application/vnd.github.v3.raw" \
    https://api.github.com/repos/fossas/circe/releases/latest > $CIRCE_RELEASE_JSON

CIRCE_TAG=$(jq -cr ".tag_name" $CIRCE_RELEASE_JSON)
echo "Using circe release: $CIRCE_TAG"

if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "cygwin" || "$OSTYPE" == "win32" ]]; then
  # Windows uses .zip files
  CIRCE_ARCHIVE_NAME="circe-$CIRCE_ASSET_POSTFIX.zip"
  echo "Downloading Windows $CIRCE_ARCHIVE_NAME"
  CIRCE_DOWNLOAD_URL="https://github.com/fossas/circe/releases/download/$CIRCE_TAG/$CIRCE_ARCHIVE_NAME"

  # Create a temporary directory for extraction
  TEMP_DIR=$(mktemp -d)
  TEMP_ZIP="$TEMP_DIR/$CIRCE_ARCHIVE_NAME"

  curl -sL -H "Authorization: token $GITHUB_TOKEN" -o "$TEMP_ZIP" "$CIRCE_DOWNLOAD_URL"

  # Extract the binary and copy to vendor-bins
  # For Windows, the binaries are directly in the archive (not in a subdirectory)
  unzip -j "$TEMP_ZIP" "circe.exe" -d "$TEMP_DIR" > /dev/null
  cp "$TEMP_DIR/circe.exe" vendor-bins/circe
  rm -rf "$TEMP_DIR"
else
  # Linux and macOS use .tar.xz files
  CIRCE_ARCHIVE_NAME="circe-$CIRCE_ASSET_POSTFIX.tar.xz"
  echo "Downloading $CIRCE_ARCHIVE_NAME"
  CIRCE_DOWNLOAD_URL="https://github.com/fossas/circe/releases/download/$CIRCE_TAG/$CIRCE_ARCHIVE_NAME"

  # Create a temporary directory for extraction
  TEMP_DIR=$(mktemp -d)
  TEMP_TAR="$TEMP_DIR/$CIRCE_ARCHIVE_NAME"

  curl -sL -H "Authorization: token $GITHUB_TOKEN" -o "$TEMP_TAR" "$CIRCE_DOWNLOAD_URL"

  # Extract the binary and copy to vendor-bins - circe files are in a subdirectory
  tar -xf "$TEMP_TAR" -C "$TEMP_DIR"
  cp "$TEMP_DIR/circe-$CIRCE_ASSET_POSTFIX/circe" vendor-bins/circe
  rm -rf "$TEMP_DIR"
fi

echo "Circe download successful"

rm $CIRCE_RELEASE_JSON

# Finished downloading

echo
echo "Marking binaries executable"
chmod +x vendor-bins/*

echo "Compressing index.gob"
xz vendor-bins/index.gob

echo "Vendored binaries are ready for use"
ls -lh vendor-bins/

echo "Checking binary versions"
for binary in vendor-bins/*; do
  if [ -x "$binary" ] && [[ "$binary" != *".xz" ]]; then
    echo -n "$(basename "$binary"): "
    "$binary" --version || echo "failed to get version information"
  fi
done

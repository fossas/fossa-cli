#!/bin/bash
# This is derived from https://github.com/jpillora/installer
# Original license notice:
# ---
# Copyright © 2016 Jaime Pillora <dev@jpillora.com>
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the 'Software'), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
# ---

TMP_DIR="/tmp/install-fossa-cli"

function cleanup {
  echo rm -rf $TMP_DIR > /dev/null
}

function fail {
  cleanup
  msg=$1
  echo "============"
  echo "Error: $msg" 1>&2
  exit 1
}

function install {
  # Settings
  USER="fossas"
  REPO="fossa-cli"
  BIN="fossa"
  # TODO: automatically get latest version from GitHub Releases API
  VERSION="0.4.1"
  RELEASE="v$VERSION"
  MOVE="true"
  INSECURE="false"
  OUT_DIR="/usr/local/bin"
  GH="https://github.com"

  # `bash` check
  [ ! "$BASH_VERSION" ] && fail "Please use bash instead"
  [ ! -d $OUT_DIR ] && fail "output directory missing: $OUT_DIR"

  # Check for non-POSIX dependencies
  GET=""
  if which curl > /dev/null; then
    GET="curl"
    if [[ $INSECURE = "true" ]]; then GET="$GET --insecure"; fi
    GET="$GET --fail -# -L"
  elif which wget > /dev/null; then
    GET="wget"
    if [[ $INSECURE = "true" ]]; then GET="$GET --no-check-certificate"; fi
    GET="$GET -qO-"
  else
    fail "neither wget nor curl are installed"
  fi
  which sha256sum > /dev/null || fail "sha256sum is not installed"
  which tar > /dev/null || fail "tar is not installed"
  which gzip > /dev/null || fail "gzip is not installed"

  # Detect OS
  case `uname -s` in
    Darwin) OS="darwin";;
    Linux) OS="linux";;
    *) fail "unknown os: $(uname -s)";;
  esac

  # Detect architecture
  if uname -m | grep 64 > /dev/null; then
    ARCH="amd64"
  elif uname -m | grep arm > /dev/null; then
    ARCH="arm"
  elif uname -m | grep 386 > /dev/null; then
    ARCH="386"
  else
    fail "unknown arch: $(uname -m)"
  fi

  # Fail for unsupported OS/architecture combinations
  case "${OS}_${ARCH}" in
  "darwin_amd64") ;;
  "linux_amd64") ;;
  "windows_amd64") ;;
  *) fail "No asset for platform ${OS}-${ARCH}";;
  esac


  # Enter temporary directory
  echo "Installing $USER/$REPO $RELEASE..."
  mkdir -p $TMP_DIR
  cd $TMP_DIR

  # Download and validate release
  RELEASE_URL="$GH/$USER/$REPO/releases/download/$RELEASE"
  bash -c "$GET $RELEASE_URL/${REPO}_${VERSION}_${OS}_${ARCH}.tar.gz" > release.tar.gz || fail "downloading release failed"
  bash -c "$GET $RELEASE_URL/${REPO}_${VERSION}_checksums.txt" > checksums.txt || fail "downloading checksums failed"
  [ "$(grep "${OS}_${ARCH}" checksums.txt | cut -f 1 -d " ")" == "$(sha256sum release.tar.gz | cut -f 1 -d " ")" ] || fail "invalid checksum"

  # Extract release
  tar zxf release.tar.gz || fail "tar failed"
  rm release.tar.gz

  # Move binary into output directory
  chmod +x $BIN || fail "chmod +x failed"
  mv $BIN $OUT_DIR/$BIN || fail "mv failed"
  echo "Installed at $OUT_DIR/$BIN"

  cleanup
}

install

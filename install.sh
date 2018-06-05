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
  rm -rf $TMP_DIR > /dev/null
}
trap cleanup EXIT

function fail {
  msg=$1
  echo "============"
  echo "Error: $msg" 1>&2
  exit 1
}

# This function will ask for root privileges before executing a command
# The goal is to allow the user to run this script as a normal user and
# to be asked for authorizations as needed
function askRoot {
  if [ $(id -u) -eq 0 ]; then
    "$@"
  else
    echo "The following command needs administrator privileges:"
    echo
    echo -e "\\t$*"
    echo
    # The -k flag forces sudo to re-ask the user for their authorization
    if command -v sudo > /dev/null; then
      sudo -k "$@"
    elif command -v su > /dev/null; then
      su root -c "/bin/bash $@"
    else
      fail "neither sudo nor su are installed"
    fi
  fi
}

function install {
  # Settings
  USER="fossas"
  REPO="fossa-cli"
  BIN="fossa"
  INSECURE="false"
  OUT_DIR="/usr/local/bin"
  GH="https://github.com"
  GH_API="https://api.github.com"

  # `bash` check
  [ ! "$BASH_VERSION" ] && fail "Please use bash instead"
  [ ! -d $OUT_DIR ] && fail "output directory missing: $OUT_DIR"

  # Check for non-POSIX dependencies
  GET=""
  if command -v curl > /dev/null; then
    GET="curl"
    if [[ $INSECURE = "true" ]]; then GET="$GET --insecure"; fi
    GET="$GET --fail -# -L"
  elif command -v wget > /dev/null; then
    GET="wget"
    if [[ $INSECURE = "true" ]]; then GET="$GET --no-check-certificate"; fi
    GET="$GET -qO-"
  else
    fail "neither wget nor curl are installed"
  fi
  command -v tar > /dev/null || fail "tar is not installed"
  command -v gzip > /dev/null || fail "gzip is not installed"

  # Detect OS
  case $(uname -s) in
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
  mkdir -p $TMP_DIR
  cd $TMP_DIR || fail "changing directory to $TMP_DIR failed"

  # Download and validate release
  bash -c "$GET $GH_API/repos/$USER/$REPO/releases/latest" > latest || fail "downloading latest release metadata failed"
  RELEASE=$(grep tag_name latest | cut -d'"' -f4)
  VERSION=${RELEASE#v} # remove prefix "v"

  echo "Installing $USER/$REPO $RELEASE..."
  RELEASE_URL="$GH/$USER/$REPO/releases/download/$RELEASE"
  bash -c "$GET $RELEASE_URL/${REPO}_${VERSION}_${OS}_${ARCH}.tar.gz" > release.tar.gz || fail "downloading release failed"
  bash -c "$GET $RELEASE_URL/${REPO}_${VERSION}_checksums.txt" > checksums.txt || fail "downloading checksums failed"
  # TODO: checksums are not actually validated. We need to check with `sha256sum` on Linux and `shasum -a 256` on MacOS.

  # Extract release
  tar zxf release.tar.gz || fail "tar failed"
  rm release.tar.gz

  # Move binary into output directory
  chmod +x $BIN || fail "chmod +x failed"

  # Admin privileges are required to run this command
  askRoot mv $BIN $OUT_DIR/$BIN || fail "mv failed"
  echo "Installed at $OUT_DIR/$BIN"
}

install

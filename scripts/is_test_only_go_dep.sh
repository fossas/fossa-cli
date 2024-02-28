#!/bin/sh

# This script returns 0 if it thinks the dep is a test-only dep, 1 otherwise.

USAGE="usage: ./go_test_only_dep.sh <project directory> <Go Package>"

if [ $# -lt 2 ]; then
    echo "$USAGE"
    exit 1
fi

PROJ_DIR=$1
PACKAGE=$2

# Here we exclude any directories that look like vendored code.
# If they have vendored code in other directories, this could mean there's a false positive.
IMPORTS=$(find "$PROJ_DIR" -not -path '*vendor*' -name \*.go -exec grep -Hn "$PACKAGE" {} \;)

if [ "$IMPORTS" = '' ]; then
    echo "Found no references to package \"$PACKAGE\" in $PROJ_DIR"
    exit 1
fi

# -v inverts the match, printing any lines that DON'T match and returning 0 on matches found, 1 otherwise.
# Any line match for the package in a file that DOES NOT have a '_test.go' string in the name is printed.
# It's a convoluted way to figure out if any of the files found referencing the package are not _test.go files.
# SH is the pits.
TEST_LINES=$(echo "$IMPORTS" | grep -v "_test.go")
HAS_TEST_LINES="$?"

# Edge case to be aware of:
# This doesn't actually parse Go source files, so even a commented reference to a package counts as a "hit".
if [ "$HAS_TEST_LINES" = "0" ] ; then
    echo "$TEST_LINES" | while read -r i; do
        FILE=$(echo "$i" | awk 'BEGIN { FS=":" }; {print $1}')
        echo "Found package \"$PACKAGE\" in non-test file: $FILE"
        exit 1
    done
else
    echo "Package \"$PACKAGE\" not found in any non-*_test.go file."
    echo "It is likely a test-only package."
    exit 0
fi

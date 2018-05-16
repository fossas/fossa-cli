package version

import (
	"strings"
	"testing"
)

func setDevVersions() {
	buildType = "development"
	version = "some-branch-name"
	commit = "abcdef"
	goversion = "go version go1.10.2 linux/amd64"
}

func setProdVersion() {
	buildType = "production"
	version = "v1.2.3"
	commit = "abcdef"
	goversion = "go version go1.10.2 linux/amd64"
}

func testShortStringSpaces(t *testing.T) {
	if s := ShortString(); len(strings.Fields(s)) > 1 {
		t.Errorf("ShortString() had whitespace: %#v", s)
	}
}

func TestShortStringHasNoSpaces(t *testing.T) {
	setDevVersions()
	testShortStringSpaces(t)
	setProdVersion()
	testShortStringSpaces(t)
}

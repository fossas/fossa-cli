package version

import (
	"strings"
	"testing"
)

type versions struct {
	name string

	buildType string
	version   string
	commit    string
	goversion string
}

var noLdFlags = versions{
	name: "NoLdFlags",

	buildType: "",
	version:   "",
	commit:    "",
	goversion: "",
}

var dev = versions{
	name: "Development",

	buildType: "development",
	version:   "some-branch-name",
	commit:    "12345abcdef",
	goversion: "go version go1.10.2 linux/amd64",
}

var prod = versions{
	name: "Release",

	buildType: "release",
	version:   "v1.2.3-validsemanticversion",
	commit:    "67890foobar",
	goversion: "go version go1.10.2 linux/amd64",
}

func testShortStringSpaces(t *testing.T) {
	if s := ShortString(); len(strings.Fields(s)) > 1 {
		t.Errorf("ShortString() had whitespace: %#v", s)
	}
}

func TestShortStringHasNoSpaces(t *testing.T) {
	testCases := []versions{
		noLdFlags, dev, prod,
	}
	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			if s := ShortString(); len(strings.Fields(s)) > 1 {
				t.Errorf("ShortString() had whitespace: %#v", s)
			}
		})
	}
}

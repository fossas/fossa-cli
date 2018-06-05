package version

import (
	"errors"
	"fmt"
	"strings"

	"github.com/blang/semver"
)

// These are set by ldflags in the Makefile and goreleaser. They may be empty
// if built without the ldflags set (e.g. by using `go get`).
var (
	buildType string
	version   string
	commit    string
	goversion string
)

// ErrIsDevelopment is returned whenever functions are called that require a
// valid release version (generally, a valid semantic version) to parse.
var ErrIsDevelopment = errors.New("this development binary has no semantic version")

// IsDevelopment returns true if the build type is not "release", and true
// otherwise. Note that checking whether the build type is "development" is not
// sufficient, because buildType may be empty when no ldflags are used.
func IsDevelopment() bool {
	return buildType != "release"
}

// String returns a long, human-readable version string.
func String() string {
	return fmt.Sprintf("%s (revision %s compiled with %s)", version, commit, goversion)
}

// ShortString returns a machine-readable version string. If this is a release
// build, it returns the release version; otherwise, it returns the commit.
func ShortString() string {
	if IsDevelopment() {
		return commit
	}
	return version
}

// Semver returns the build's parsed release version.
func Semver() (semver.Version, error) {
	if IsDevelopment() {
		return semver.Version{}, ErrIsDevelopment
	}
	return semver.Parse(strings.TrimPrefix(version, "v"))
}

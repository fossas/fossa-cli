package version

import (
	"errors"
	"fmt"
	"strings"

	"github.com/blang/semver"
)

var (
	BuildType string
	Version   string
	Commit    string
	GoVersion string
)

var ErrIsDevelopment = errors.New("this development binary has no semantic version")

func IsDevelopment() bool {
	return BuildType == "development"
}

func String() string {
	return fmt.Sprintf("%s (revision %s compiled with %s)", Version, Commit, GoVersion)
}

func ShortString() string {
	if IsDevelopment() {
		return Commit
	}
	return Version
}

func Semver() (semver.Version, error) {
	if IsDevelopment() {
		return semver.Version{}, ErrIsDevelopment
	}
	return semver.Parse(strings.TrimPrefix(Version, "v"))
}

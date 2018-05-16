package version

import (
	"errors"
	"fmt"
	"strings"

	"github.com/blang/semver"
)

var (
	buildType string
	version   string
	commit    string
	goversion string
)

var ErrIsDevelopment = errors.New("this development binary has no semantic version")

func IsDevelopment() bool {
	return buildType == "development"
}

func String() string {
	return fmt.Sprintf("%s (revision %s compiled with %s)", version, commit, goversion)
}

func ShortString() string {
	if IsDevelopment() {
		return commit
	}
	return version
}

func Semver() (semver.Version, error) {
	if IsDevelopment() {
		return semver.Version{}, ErrIsDevelopment
	}
	return semver.Parse(strings.TrimPrefix(version, "v"))
}

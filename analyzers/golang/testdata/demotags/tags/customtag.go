// +build customtag

package tags

import (
	"github.com/blang/semver"
)

func BuildTags() {
	semver.Make("1-0-0.test")
}

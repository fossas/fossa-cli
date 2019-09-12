package pkg_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/pkg"
)

func TestTypeToString(t *testing.T) {
	testcases := []struct {
		Type pkg.Type
		str  string
	}{
		{pkg.Ant, "ant"},
		{pkg.Bower, "bower"},
		{pkg.Buck, "buck"},
		{pkg.Carthage, "cart"},
		{pkg.Clojure, "clojure"},
		{pkg.Cocoapods, "pod"},
		{pkg.Composer, "composer"},
		{pkg.Debian, "debian"},
		{pkg.Go, "go"},
		{pkg.Git, "git"},
		{pkg.Gradle, "gradle"},
		{pkg.Haskell, "haskell"},
		{pkg.Maven, "mvn"},
		{pkg.NodeJS, "npm"},
		{pkg.NuGet, "nuget"},
		{pkg.OkBuck, "okbuck"},
		{pkg.Python, "pip"},
		{pkg.Ruby, "gem"},
		{pkg.Rust, "cargo"},
		{pkg.Scala, "sbt"},
		{pkg.Raw, "archive"},
		{0, ""},
		{1000000, ""},
	}
	for _, tc := range testcases {
		assert.Equal(t, tc.str, tc.Type.String())
	}
}

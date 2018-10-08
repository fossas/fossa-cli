package dep_test

import (
	"testing"

	"github.com/fossas/fossa-cli/buildtools"
	"github.com/fossas/fossa-cli/buildtools/dep"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/stretchr/testify/assert"
)

func TestResolve(t *testing.T) {
	resolver, err := dep.New("testdata/Gopkg.lock", "testdata/Gopkg.toml")
	assert.Equal(t, err, nil)

	// Test that cat/house is not included in the manifest.
	revision, err := resolver.Resolve("cat/house")
	assert.Equal(t, err, buildtools.ErrNoRevisionForPackage)
	assert.Equal(t, revision, pkg.Import{})

	// Test that cat/puma is an ignored revision.
	revision, err = resolver.Resolve("cat/puma")
	assert.Equal(t, err, buildtools.ErrPackageIsIgnored)
	assert.Equal(t, revision, pkg.Import{})

	// Test that cat/big/lion is an ignored revision under wildcard rules.
	revision, err = resolver.Resolve("cat/big/lion")
	assert.Equal(t, err, buildtools.ErrPackageIsIgnored)
	assert.Equal(t, revision, pkg.Import{})

	// Test that cat/fossa is an accepted revision.
	revision, err = resolver.Resolve("cat/fossa")
	assert.Equal(t, err, nil)
	assert.Equal(t, revision.Target, "v0.3.0")
}

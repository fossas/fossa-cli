package vndr_test

import (
	"testing"

	"github.com/fossas/fossa-cli/buildtools"
	"github.com/fossas/fossa-cli/buildtools/vndr"
	"github.com/fossas/fossa-cli/pkg"

	"github.com/stretchr/testify/assert"
)

func TestResolve(t *testing.T) {
	resolver, err := vndr.New("testdata")
	assert.Equal(t, nil, err)

	// Test that fossas/package-one is an included package.
	revision, err := resolver.Resolve("fossas/package-one")
	assert.Equal(t, nil, err)
	assert.Equal(t, "fossas/package-one", revision.Resolved.Name)

	// Test that rsc.io/letsencrypt uses the correct location.
	revision, err = resolver.Resolve("rsc.io/letsencrypt")
	assert.Equal(t, nil, err)
	assert.Equal(t, "fossas/privatefork", revision.Resolved.Name)

	// Test that missing/package is not included.
	revision, err = resolver.Resolve("missing/package")
	assert.Equal(t, buildtools.ErrNoRevisionForPackage, err)
	assert.Equal(t, pkg.Import{}, revision)
}

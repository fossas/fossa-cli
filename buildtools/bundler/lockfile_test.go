package bundler_test

import (
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/bundler"
)

// TODO: we should make sure this is exactly correct, using a struct fixture.
func TestLockfileParsing(t *testing.T) {
	l, err := bundler.FromLockfile(filepath.Join("testdata", "Gemfile.lock"))
	assert.NoError(t, err)

	testSections(t, l.Git)
	testSections(t, l.Path)
	testSections(t, l.Gem)
	assert.NotEmpty(t, l.Dependencies)

	t.Logf("%#v", l)
}

func testSections(t *testing.T, sections []bundler.Section) {
	for _, section := range sections {
		assert.NotEmpty(t, section.Specs)
		for _, spec := range section.Specs {
			assert.NotZero(t, spec)
			assert.NotEmpty(t, spec.Name)
		}
	}
}

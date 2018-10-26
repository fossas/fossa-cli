package gomodules_test

import (
	"io/ioutil"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools"
	"github.com/fossas/fossa-cli/buildtools/gomodules"
)

func TestResolver(t *testing.T) {
	// This file is stored as txt because `go list -json` does not output valid json.
	file, err := ioutil.ReadFile(filepath.Join("testdata", "go-list-all.txt"))
	assert.NoError(t, err)

	resolver, err := gomodules.ParseModuleJSON(string(file))
	assert.NoError(t, err)
	assert.NotEmpty(t, resolver)

	revision, err := resolver.Resolve("test/basic")
	assert.NoError(t, err)
	assert.Equal(t, "test/basic", revision.Resolved.Name)
	assert.Equal(t, "v1.1.1", revision.Resolved.Revision)

	revision, err = resolver.Resolve("test/basic/with/extensions")
	assert.NoError(t, err)
	assert.Equal(t, "test/basic", revision.Resolved.Name)
	assert.Equal(t, "v1.1.1", revision.Resolved.Revision)

	revision, err = resolver.Resolve("test/versionzero")
	assert.NoError(t, err)
	assert.Equal(t, "test/versionzero", revision.Resolved.Name)
	assert.Equal(t, "123456789000", revision.Resolved.Revision)

	revision, err = resolver.Resolve("replace/fork")
	assert.NoError(t, err)
	assert.Equal(t, "private/fork", revision.Resolved.Name)
	assert.Equal(t, "v9.0.0", revision.Resolved.Revision)

	revision, err = resolver.Resolve("replace/version")
	assert.NoError(t, err)
	assert.Equal(t, "replace/version", revision.Resolved.Name)
	assert.Equal(t, "v1.0.0", revision.Resolved.Revision)

	revision, err = resolver.Resolve("test/failure")
	assert.Equal(t, buildtools.ErrNoRevisionForPackage, err)
}

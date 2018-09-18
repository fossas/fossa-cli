package dep

import (
	"testing"

	"github.com/fossas/fossa-cli/pkg"
	"github.com/stretchr/testify/assert"
)

func TestIsIgnored(t *testing.T) {
	manifest := Manifest{Ignored: []string{"apple", "orange*"}}

	// File listed in ignored list is ignored.
	valid := manifest.isIgnored("apple")
	assert.Equal(t, valid, true)

	// Wildcard entry properly ignores its own package.
	valid = manifest.isIgnored("orange")
	assert.Equal(t, valid, true)

	// Wildcard entry properly ignores other packages.
	valid = manifest.isIgnored("orange/blood")
	assert.Equal(t, valid, true)

	// File not listed in ignored list is not ignored.
	valid = manifest.isIgnored("apple/fuji")
	assert.Equal(t, valid, false)
}

func TestReadLockfile(t *testing.T) {
	// Reading a valid lockfile returns expected data.
	lock, err := readLockfile("testdata/Gopkg.lock")
	expectedLockfile := lockfile{
		Projects: []Project{
			Project{
				Name:     "cat/fossa",
				Packages: []string{"."},
				Revision: "1",
				Version:  "v0.3.0",
			},
		},
		normalized: map[string]pkg.Import{
			"cat/fossa": pkg.Import{
				Target: "v0.3.0",
				Resolved: pkg.ID{
					Type:     pkg.Go,
					Name:     "cat/fossa",
					Revision: "1",
					Location: "",
				},
			},
		},
	}

	assert.Equal(t, err, nil)
	assert.Equal(t, lock, expectedLockfile)

	// Reading an invalid lockfile returns an expected error.
	_, err = readLockfile("NotAFile")
	assert.Error(t, err)
}

func TestReadManifest(t *testing.T) {
	// Reading a valid manifest returns expected data.
	manifest, err := readManifest("testdata/Gopkg.toml")
	expectedManifest := Manifest{
		Ignored: []string{
			"cat/puma",
			"cat/big/*",
		},
	}

	assert.Equal(t, err, nil)
	assert.Equal(t, manifest, expectedManifest)

	// Reading an invalid manifest returns an expected error.
	_, err = readManifest("NotAFile")
	assert.Error(t, err)
}

package dep

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/pkg"
)

func TestIsIgnored(t *testing.T) {
	man := manifest{Ignored: []string{"apple", "orange*"}}

	// File listed in ignored list is ignored.
	valid := man.isIgnored("apple")
	assert.Equal(t, valid, true)

	// Wildcard entry properly ignores its own package.
	valid = man.isIgnored("orange")
	assert.Equal(t, valid, true)

	// Wildcard entry properly ignores other packages.
	valid = man.isIgnored("orange/blood")
	assert.Equal(t, valid, true)

	// File not listed in ignored list is not ignored.
	valid = man.isIgnored("apple/fuji")
	assert.Equal(t, valid, false)
}

func TestReadLockfile(t *testing.T) {
	// Reading a valid lockfile returns expected data.
	lock, err := readLockfile("testdata/Ignore.lock")
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
					Revision: "v0.3.0",
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
	man, err := readManifest("testdata/Ignore.toml")
	expectedManifest := manifest{
		Ignored: []string{
			"cat/puma",
			"cat/big/*",
		},
	}

	assert.Equal(t, err, nil)
	assert.Equal(t, man, expectedManifest)

	// Reading an invalid manifest returns an expected error.
	_, err = readManifest("NotAFile")
	assert.Error(t, err)
}

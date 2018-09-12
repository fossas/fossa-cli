package npm

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFromManifest(t *testing.T) {
	manifest, err := FromManifest("testdata/package.json")
	assert.NoError(t, err)

	assert.NotEmpty(t, manifest.Dependencies)
	assert.Equal(t, manifest.Dependencies["chai"], "4.1.2")
}

func TestFromNodeModules(t *testing.T) {
	t.Skip("not yet implemented")
	manifests, err := FromNodeModules("testdata/package.json")
	assert.NoError(t, err)

	/*
		└─┬ chai@4.1.2
			├── assertion-error@1.1.0
			├── check-error@1.0.2
			├─┬ deep-eql@3.0.1
			│ └── type-detect@4.0.8 deduped
			├── get-func-name@2.0.0
			├── pathval@1.1.0
			└── type-detect@4.0.8
	*/

	assert.NotEmpty(t, manifests)
	assert.True(t, containsDep(manifests, "assertion-error", "1.1.0"), "Manifests does not include dep assertion-error")
	assert.True(t, containsDep(manifests, "check-error", "1.0.2"), "Manifests does not include dep check-error")
	assert.True(t, containsDep(manifests, "get-func-name", "2.0.0"), "Manifests does not include dep get-func-name")
	assert.True(t, containsDep(manifests, "pathval", "1.1.0"), "Manifests does not include dep pathval")
	assert.True(t, containsDep(manifests, "type-detect", "4.0.8"), "Manifests does not include dep type-detect")

	// check transitive dep's existance
	dep, err := selectDep(manifests, "deep-eql", "3.0.1")
	assert.NoError(t, err, "Manifests does not include dep deep-eql")

	assert.NotEmpty(t, dep.Dependencies)
	assert.Len(t, dep.Dependencies, 1)
	assert.Contains(t, dep.Dependencies, "type-detect")
}

func selectDep(manifests []Manifest, name string, version string) (Manifest, error) {
	for _, v := range manifests {
		if v.Name == name && v.Version == version {
			return v, nil
		}
	}

	return Manifest{}, errors.New("could not find manifest")
}

func containsDep(manifests []Manifest, name string, version string) bool {
	_, err := selectDep(manifests, name, version)
	return err == nil
}

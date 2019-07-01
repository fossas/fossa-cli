package maven_test

import (
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/maven"
	"github.com/fossas/fossa-cli/pkg"
)

func TestPomFileGraph(t *testing.T) {
	// This test needs just the Dependencies field of a Manifest to be translated to a graph.Deps.
	projectDir := testdataDir + "/nested"
	projectTarget := "deep-nested/pom.xml"

	id1 := pkg.ID{Type: pkg.Maven, Name: "com.google.code.g:g", Revision: "2.7"}
	id2 := pkg.ID{Type: pkg.Maven, Name: "javax.annotation:jsr250-api", Revision: "1.0.2"}
	id3 := pkg.ID{Type: pkg.Maven, Name: "junit:junit", Revision: "3.8.2"}

	wantDirect := []pkg.Import{
		{Target: "", Resolved: id1},
		{Target: "", Resolved: id2},
		{Target: "", Resolved: id3},
	}
	wantTransitive := map[pkg.ID]pkg.Package{
		id1: {ID: id1},
		id2: {ID: id2},
		id3: {ID: id3},
	}

	got, err := maven.PomFileGraph(projectTarget, projectDir)
	assert.NoError(t, err)
	assert.Equal(t, wantDirect, got.Direct)
	assert.Equal(t, wantTransitive, got.Transitive)
}

func TestResolveManifestFromTarget(t *testing.T) {
	// A directory path.
	pom, err := maven.ResolveManifestFromTarget(testdataDir, ".")
	if assert.NoError(t, err) {
		assert.Equal(t, "com.domain.name:stuff", pom.GroupID+":"+pom.ArtifactID)
	}

	// A POM file path.
	pom2, err := maven.ResolveManifestFromTarget(filepath.Join(testdataDir, "nested", "pom.xml"), ".")
	if assert.NoError(t, err) {
		assert.Equal(t, "com.someone.code.a:gson-extras", pom2.GroupID+":"+pom2.ArtifactID)
	}

	// A project identifier.
	_, err3 := maven.ResolveManifestFromTarget("something:else", ".")
	assert.Error(t, err3)
}

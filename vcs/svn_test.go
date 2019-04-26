package vcs

import (
	"fmt"
	"io/ioutil"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestNewSubversionRepository(t *testing.T) {
	if testing.Short() {
		t.Skip("Skip integration test")
	}

	dir := filepath.Join("testdata", "subversion")
	repo, err := NewSubversionRepository(dir)
	assert.NoError(t, err)

	var project string
	assert.NotPanics(t, func() {
		project = repo.Project()
	})

	assert.Equal(t, "file:///tmp/demo", project)
	head := repo.Head()
	assert.Equal(t, "trunk", head.Branch)
	assert.Equal(t, "0", head.RevisionID)
}

func TestSvnBranchFromInfo(t *testing.T) {
	// info has only the fields that this function considers.
	type info struct {
		URL, relativeURL, repoRoot string
	}
	cases := []struct {
		info
		branch string
	}{
		{
			info{URL: "https://svn.a.com/proj", relativeURL: "^/proj", repoRoot: "https://svn.a.com"},
			"trunk",
		},
		{
			info{URL: "https://a.com/proj/branches/b2", relativeURL: "^/proj", repoRoot: "https://a.com"},
			"b2",
		},
		{
			info{URL: "https://b.co/my-proj", relativeURL: "^/", repoRoot: "https://b.co/my-proj",},
			"trunk",
		},
	}
	for i, tc := range cases {
		t.Run(fmt.Sprintf("case_%d", i), func(t *testing.T) {
			var info svnInfo
			info.Entry.URL = tc.info.URL
			info.Entry.RelativeURL = tc.info.relativeURL
			info.Entry.Repository.Root = tc.info.repoRoot
			branch := svnBranchFromInfo(&info)
			assert.Equal(t, tc.branch, branch)
		})
	}
}

func TestSvnInfo_unmarshalXML(t *testing.T) {
	data, err := ioutil.ReadFile(filepath.Join("testdata", "svn-info.xml"))
	assert.NoError(t, err)

	var s svnInfo
	err = s.unmarshalXML(data)
	assert.NoError(t, err)

	assert.Equal(t, s.Entry.Revision, "2071031")
	assert.Equal(t, s.Entry.Kind, "dir")
	assert.Equal(t, s.Entry.URL, "https://plugins.svn.wordpress.org/wp-stateless")
	assert.Equal(t, s.Entry.Repository.Root, "https://plugins.svn.wordpress.org")
}

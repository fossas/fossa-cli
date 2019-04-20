package vcs

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

const svnInfoXML = `<?xml version="1.0" encoding="UTF-8"?>
<info>
<entry
   path="."
   revision="2071031"
   kind="dir">
<url>https://plugins.svn.wordpress.org/wp-stateless</url>
<relative-url>^/wp-stateless</relative-url>
<repository>
<root>https://plugins.svn.wordpress.org</root>
<uuid>b8457f37-d9ea-0310-8a92-e5e31aec5664</uuid>
</repository>
<wc-info>
<wcroot-abspath>/Users/u/sample-apps/wps</wcroot-abspath>
<schedule>normal</schedule>
<depth>infinity</depth>
</wc-info>
</entry>
</info>`

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
			info{
				URL:         "https://svn.a.com/proj",
				relativeURL: "^/proj",
				repoRoot:    "https://svn.a.com",
			},
			"trunk",
		},
		{
			info{
				URL:         "https://svn.a.com/proj/branches/my-branch",
				relativeURL: "^/proj",
				repoRoot:    "https://svn.a.com",
			},
			"my-branch",
		},
		{
			info{
				URL:         "https://b.co/my-proj",
				relativeURL: "^/",
				repoRoot:    "https://b.co/my-proj",
			},
			"trunk",
		},
	}
	for i := range cases {
		t.Run(fmt.Sprintf("case_%d", i), func(t *testing.T) {
			tc := &cases[i]
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
	var s svnInfo
	err := s.unmarshalXML([]byte(svnInfoXML))
	assert.NoError(t, err)

	assert.Equal(t, s.Entry.Revision, "2071031")
	assert.Equal(t, s.Entry.Kind, "dir")
	assert.Equal(t, s.Entry.URL, "https://plugins.svn.wordpress.org/wp-stateless")
	assert.Equal(t, s.Entry.Repository.Root, "https://plugins.svn.wordpress.org")
}

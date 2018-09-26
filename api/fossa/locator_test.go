package fossa_test

import (
	"testing"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/stretchr/testify/assert"
)

func TestLocatorFetcher(t *testing.T) {
	testcases := []struct {
		Type    pkg.Type
		Fetcher string
	}{
		{pkg.Ant, "mvn"},
		{pkg.Bower, "bower"},
		{pkg.Cocoapods, "pod"},
		{pkg.Composer, "comp"},
		{pkg.Go, "go"},
		{pkg.Git, "git"},
		{pkg.Gradle, "mvn"},
		{pkg.Maven, "mvn"},
		{pkg.NodeJS, "npm"},
		{pkg.NuGet, "nuget"},
		{pkg.Python, "pip"},
		{pkg.Ruby, "gem"},
		{pkg.Scala, "mvn"},
		{pkg.Raw, "archive"},
	}
	for _, tc := range testcases {
		id := pkg.ID{
			Type: tc.Type,
		}
		assert.Equal(t, tc.Fetcher, fossa.LocatorOf(id).Fetcher, tc.Fetcher)
	}
}

func TestStringGit(t *testing.T) {
	ssh := fossa.Locator{
		Fetcher:  "git",
		Project:  "git@github.com:fossas/fossa-cli.git",
		Revision: "SHAVALUE",
	}

	http := fossa.Locator{
		Fetcher:  "git",
		Project:  "http://github.com/fossas/fossa-cli.git",
		Revision: "SHAVALUE",
	}

	sshActual := ssh.String()
	httpActual := http.String()
	expected := "git+github.com/fossas/fossa-cli$SHAVALUE"
	assert.Equal(t, sshActual, expected)
	assert.Equal(t, httpActual, expected)
}

func TestStringCustom(t *testing.T) {
	fossa.MockOrgID = "3000"
	custom := fossa.Locator{
		Fetcher:  "custom",
		Project:  "fossa-cli",
		Revision: "SHAVALUE",
	}

	customActual := custom.String()
	expected := "custom+fossa-cli$SHAVALUE"
	assert.Equal(t, customActual, expected)
}

func TestStringCustomGitHTTP(t *testing.T) {
	http := fossa.Locator{
		Fetcher:  "custom",
		Project:  "http://github.com/fossa/fossa-cli.git",
		Revision: "SHAVALUE",
	}

	stringified := http.String()
	expected := "custom+http://github.com/fossa/fossa-cli.git$SHAVALUE"
	assert.Equal(t, stringified, expected)
}

func TestStringCustomGitSSH(t *testing.T) {
	ssh := fossa.Locator{
		Fetcher:  "custom",
		Project:  "git@github.com:fossa/fossa-cli.git",
		Revision: "SHAVALUE",
	}

	stringified := ssh.String()
	expected := "custom+git@github.com:fossa/fossa-cli.git$SHAVALUE"
	assert.Equal(t, stringified, expected)
}

func TestOrgStringGit(t *testing.T) {
	ssh := fossa.Locator{
		Fetcher:  "git",
		Project:  "git@github.com:fossas/fossa-cli.git",
		Revision: "SHAVALUE",
	}

	http := fossa.Locator{
		Fetcher:  "git",
		Project:  "http://github.com/fossas/fossa-cli.git",
		Revision: "SHAVALUE",
	}

	sshActual := ssh.OrgString()
	httpActual := http.OrgString()
	expected := "git+github.com/fossas/fossa-cli$SHAVALUE"
	assert.Equal(t, sshActual, expected)
	assert.Equal(t, httpActual, expected)
}

func TestOrgStringCustom(t *testing.T) {
	fossa.MockOrgID = "3000"
	git := fossa.Locator{
		Fetcher:  "custom",
		Project:  "fossa-cli",
		Revision: "SHAVALUE",
	}

	stringified := git.OrgString()
	expected := "custom+3000/fossa-cli$SHAVALUE"
	assert.Equal(t, stringified, expected)
}

func TestOrgStringCustomGitHTTP(t *testing.T) {
	fossa.MockOrgID = "3000"
	http := fossa.Locator{
		Fetcher:  "custom",
		Project:  "http://github.com/fossa/fossa-cli.git",
		Revision: "SHAVALUE",
	}

	stringified := http.OrgString()
	expected := "custom+3000/github.com/fossa/fossa-cli$SHAVALUE"
	assert.Equal(t, stringified, expected)
}

func TestOrgStringCustomGitSSH(t *testing.T) {
	fossa.MockOrgID = "3000"
	ssh := fossa.Locator{
		Fetcher:  "custom",
		Project:  "git@github.com:fossa/fossa-cli.git",
		Revision: "SHAVALUE",
	}

	stringified := ssh.OrgString()
	expected := "custom+3000/git@github.com:fossa/fossa-cli.git$SHAVALUE"
	assert.Equal(t, stringified, expected)
}

func TestURL(t *testing.T) {
	git := fossa.Locator{
		Fetcher:  "git",
		Project:  "git@github.com:fossas/fossa-cli.git",
		Revision: "SHAVALUE",
	}

	stringified := git.URL()
	expected := "https://app.fossa.io/projects/git+git@github.com:fossas%2Ffossa-cli.git/refs/branch/master/SHAVALUE"
	assert.Equal(t, stringified, expected)
}

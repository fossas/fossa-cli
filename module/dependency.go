package module

import "strings"

// Locator is a string specifying a particular dependency and revision
type Locator string

// Dependency represents a code library brought in by running a Build
type Dependency interface {
	Fetcher() string
	Package() string
	Revision() string
}

func DepLocator(dep Dependency) Locator {
	return MakeLocator(dep.Fetcher(), dep.Package(), dep.Revision())
}

func IsResolved(dep Dependency) bool {
	return dep.Revision() != ""
}

func normalizeGitURL(project string) string {
	// Remove fetcher prefix (in case project is derived from splitting a locator on '$')
	noFetcherPrefix := strings.TrimPrefix(project, "git+")

	// Normalize Git URL format
	noGitExtension := strings.TrimSuffix(noFetcherPrefix, ".git")
	handleGitHubSSH := strings.Replace(noGitExtension, "git@github.com:", "github.com/", 1)

	// Remove protocols
	noHTTPPrefix := strings.TrimPrefix(handleGitHubSSH, "http://")
	noHTTPSPrefix := strings.TrimPrefix(noHTTPPrefix, "https://")

	return noHTTPSPrefix
}

// MakeLocator creates a locator string given a package and revision
func MakeLocator(fetcher string, project string, revision string) Locator {
	if fetcher != "git" {
		return Locator(fetcher + "+" + project + "$" + revision)
	}
	return Locator("git+" + normalizeGitURL(project) + "$" + revision)
}

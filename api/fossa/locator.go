package fossa

import (
	"regexp"
	"strings"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/pkg"
)

// Locator serializes FOSSA API locators.
type Locator struct {
	Fetcher  string `json:"fetcher"`
	Project  string `json:"package"`
	Revision string `json:"revision"`
}

func (l Locator) String() string {
	if l.Fetcher != "git" {
		if l.Fetcher == "archive" {
			orgID, err := GetOrganizationID()
			if err != nil {
				log.Warnf("Could not get OrganizationID while constructing locator")
			}
			l.Project = orgID + "/" + l.Project
		}
		return l.Fetcher + "+" + l.Project + "$" + l.Revision
	}
	return "git+" + NormalizeGitURL(l.Project) + "$" + l.Revision
}

// NormalizeGitURL normalizes all forms of git remote URLs to a single standard
// form.
func NormalizeGitURL(project string) string {
	// Remove fetcher prefix (in case project is derived from splitting a locator on '$').
	noFetcherPrefix := strings.TrimPrefix(project, "git+")

	// Normalize Git URL format.
	noGitExtension := strings.TrimSuffix(noFetcherPrefix, ".git")
	handleGitHubSSH := strings.Replace(noGitExtension, "git@github.com:", "github.com/", 1)

	// Remove protocols
	noHTTPPrefix := strings.TrimPrefix(handleGitHubSSH, "http://")
	noHTTPSPrefix := strings.TrimPrefix(noHTTPPrefix, "https://")

	return noHTTPSPrefix
}

// IsResolved returns true only if a locator is resolved.
func (l Locator) IsResolved() bool {
	return l.Revision != ""
}

// ReadLocator parses a string locator into a Locator.
func ReadLocator(locator string) Locator {
	locatorRegexp := regexp.MustCompile("^(.*?)\\+(.*?)\\$(.*?)$")
	matches := locatorRegexp.FindStringSubmatch(locator)
	return Locator{
		Fetcher:  matches[1],
		Project:  matches[2],
		Revision: matches[3],
	}
}

// LocatorOf returns the locator of a pkg.ID.
func LocatorOf(id pkg.ID) Locator {
	// TODO: maybe this should panic?
	if id.Type == pkg.Invalid {
		log.Warnf("Unrecognized locator")
		return Locator{}
	}
	// Normalize locator fetchers.
	fetcher := id.Type.String()
	switch id.Type {
	case pkg.Composer:
		fetcher = "comp"
	case pkg.Gradle:
		fetcher = "mvn"
	case pkg.Ant:
		fetcher = "mvn"
	case pkg.Scala:
		fetcher = "mvn"
	}

	return Locator{
		Fetcher:  fetcher,
		Project:  id.Name,
		Revision: id.Revision,
	}
}

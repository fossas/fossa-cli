package selfupdate

import (
	"fmt"
	"regexp"
	"runtime"
	"strings"

	"github.com/blang/semver"
	"github.com/google/go-github/github"
)

var reVersion = regexp.MustCompile(`\d+\.\d+\.\d+`)

func findAssetFromReleasse(rel *github.RepositoryRelease, suffixes []string) (*github.ReleaseAsset, semver.Version, bool) {
	if rel.GetDraft() {
		log.Println("Skip draft version", rel.GetTagName())
		return nil, semver.Version{}, false
	}
	if rel.GetPrerelease() {
		log.Println("Skip pre-release version", rel.GetTagName())
		return nil, semver.Version{}, false
	}

	verText := rel.GetTagName()
	indices := reVersion.FindStringIndex(verText)
	if indices == nil {
		log.Println("Skip version not adopting semver", verText)
		return nil, semver.Version{}, false
	}
	if indices[0] > 0 {
		log.Println("Strip prefix of version", verText[:indices[0]], "from", verText)
		verText = verText[indices[0]:]
	}

	// If semver cannot parse the version text, it means that the text is not adopting
	// the semantic versioning. So it should be skipped.
	ver, err := semver.Make(verText)
	if err != nil {
		log.Println("Failed to parse a semantic version", verText)
		return nil, semver.Version{}, false
	}

	for _, asset := range rel.Assets {
		name := asset.GetName()
		for _, s := range suffixes {
			if strings.HasSuffix(name, s) {
				return &asset, ver, true
			}
		}
	}

	log.Println("No suitable asset was found in release", rel.GetTagName())
	return nil, semver.Version{}, false
}

func findReleaseAndAsset(rels []*github.RepositoryRelease) (*github.RepositoryRelease, *github.ReleaseAsset, semver.Version, bool) {
	// Generate valid suffix candidates to match
	suffixes := make([]string, 0, 2*7*2)
	for _, sep := range []rune{'_', '-'} {
		for _, ext := range []string{".zip", ".tar.gz", ".gzip", ".gz", ".tar.xz", ".xz", ""} {
			suffix := fmt.Sprintf("%s%c%s%s", runtime.GOOS, sep, runtime.GOARCH, ext)
			suffixes = append(suffixes, suffix)
			if runtime.GOOS == "windows" {
				suffix = fmt.Sprintf("%s%c%s.exe%s", runtime.GOOS, sep, runtime.GOARCH, ext)
				suffixes = append(suffixes, suffix)
			}
		}
	}

	var ver semver.Version
	var asset *github.ReleaseAsset
	var release *github.RepositoryRelease

	// Find the latest version from the list of releases.
	// Returned list from GitHub API is in the order of the date when created.
	//   ref: https://github.com/rhysd/go-github-selfupdate/issues/11
	for _, rel := range rels {
		if a, v, ok := findAssetFromReleasse(rel, suffixes); ok {
			// Note: any version with suffix is less than any version without suffix.
			// e.g. 0.0.1 > 0.0.1-beta
			if release == nil || v.GTE(ver) {
				ver = v
				asset = a
				release = rel
			}
		}
	}

	if release == nil {
		log.Println("Could not find any release for", runtime.GOOS, "and", runtime.GOARCH)
		return nil, nil, semver.Version{}, false
	}

	return release, asset, ver, true
}

// DetectLatest tries to get the latest version of the repository on GitHub. 'slug' means 'owner/name' formatted string.
// It fetches releases information from GitHub API and find out the latest release with matching the tag names and asset names.
// Drafts and pre-releases are ignored. Assets whould be suffixed by the OS name and the arch name such as 'foo_linux_amd64'
// where 'foo' is a command name. '-' can also be used as a separator. File can be compressed with zip, gzip, zxip, tar&zip or tar&zxip.
// So the asset can have a file extension for the corresponding compression format such as '.zip'.
// On Windows, '.exe' also can be contained such as 'foo_windows_amd64.exe.zip'.
func (up *Updater) DetectLatest(slug string) (*Release, bool, error) {
	repo := strings.Split(slug, "/")
	if len(repo) != 2 || repo[0] == "" || repo[1] == "" {
		return nil, false, fmt.Errorf("Invalid slug format. It should be 'owner/name': %s", slug)
	}

	rels, res, err := up.api.Repositories.ListReleases(up.apiCtx, repo[0], repo[1], nil)
	if err != nil {
		log.Println("API returned an error response:", err)
		if res != nil && res.StatusCode == 404 {
			// 404 means repository not found or release not found. It's not an error here.
			err = nil
			log.Println("API returned 404. Repository or release not found")
		}
		return nil, false, err
	}

	rel, asset, ver, found := findReleaseAndAsset(rels)
	if !found {
		return nil, false, nil
	}

	url := asset.GetBrowserDownloadURL()
	log.Println("Successfully fetched the latest release. tag:", rel.GetTagName(), ", name:", rel.GetName(), ", URL:", rel.GetURL(), ", Asset:", url)

	publishedAt := rel.GetPublishedAt().Time
	release := &Release{
		ver,
		url,
		asset.GetSize(),
		asset.GetID(),
		rel.GetHTMLURL(),
		rel.GetBody(),
		rel.GetName(),
		&publishedAt,
		repo[0],
		repo[1],
	}
	return release, true, nil
}

// DetectLatest detects the latest release of the slug (owner/repo).
// This function is a shortcut version of updater.DetectLatest() method.
func DetectLatest(slug string) (*Release, bool, error) {
	return DefaultUpdater().DetectLatest(slug)
}

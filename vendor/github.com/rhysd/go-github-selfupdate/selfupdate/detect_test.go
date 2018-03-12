package selfupdate

import (
	"fmt"
	"github.com/blang/semver"
	"os"
	"strings"
	"testing"
)

func TestDetectReleaseWithVersionPrefix(t *testing.T) {
	r, ok, err := DetectLatest("rhysd/github-clone-all")
	if err != nil {
		t.Fatal("Fetch failed:", err)
	}
	if !ok {
		t.Fatal("Failed to detect latest")
	}
	if r == nil {
		t.Fatal("Release detected but nil returned for it")
	}
	if r.Version.LE(semver.MustParse("2.0.0")) {
		t.Error("Incorrect version:", r.Version)
	}
	if !strings.HasSuffix(r.AssetURL, ".zip") && !strings.HasSuffix(r.AssetURL, ".tar.gz") {
		t.Error("Incorrect URL for asset:", r.AssetURL)
	}
	if r.URL == "" {
		t.Error("Document URL should not be empty")
	}
	if r.ReleaseNotes == "" {
		t.Error("Description should not be empty for this repo")
	}
	if r.Name == "" {
		t.Error("Release name is unexpectedly empty")
	}
	if r.AssetByteSize == 0 {
		t.Error("Asset's size is unexpectedly zero")
	}
	if r.AssetID == 0 {
		t.Error("Asset's ID is unexpectedly zero")
	}
	if r.PublishedAt.IsZero() {
		t.Error("Release time is unexpectedly zero")
	}
	if r.RepoOwner != "rhysd" {
		t.Error("Repo owner is not correct:", r.RepoOwner)
	}
	if r.RepoName != "github-clone-all" {
		t.Error("Repo name was not properly detectd:", r.RepoName)
	}
}

func TestDetectReleasesForVariousArchives(t *testing.T) {
	for _, tc := range []struct {
		slug   string
		prefix string
	}{
		{"rhysd-test/test-release-zip", "v"},
		{"rhysd-test/test-release-tar", "v"},
		{"rhysd-test/test-release-gzip", "v"},
		{"rhysd-test/test-release-xz", "release-v"},
		{"rhysd-test/test-release-tar-xz", "release-"},
	} {
		t.Run(tc.slug, func(t *testing.T) {
			r, ok, err := DetectLatest(tc.slug)
			if err != nil {
				t.Fatal("Fetch failed:", err)
			}
			if !ok {
				t.Fatal(tc.slug, "not found")
			}
			if r == nil {
				t.Fatal("Release not detected")
			}
			if !r.Version.Equals(semver.MustParse("1.2.3")) {
				t.Error("")
			}
			url := fmt.Sprintf("https://github.com/%s/releases/tag/%s1.2.3", tc.slug, tc.prefix)
			if r.URL != url {
				t.Error("URL is not correct. Want", url, "but got", r.URL)
			}
			if r.ReleaseNotes == "" {
				t.Error("Release note is unexpectedly empty")
			}
			if !strings.HasPrefix(r.AssetURL, fmt.Sprintf("https://github.com/%s/releases/download/%s1.2.3/", tc.slug, tc.prefix)) {
				t.Error("Unexpected asset URL:", r.AssetURL)
			}
			if r.Name == "" {
				t.Error("Release name is unexpectedly empty")
			}
			if r.AssetByteSize == 0 {
				t.Error("Asset's size is unexpectedly zero")
			}
			if r.AssetID == 0 {
				t.Error("Asset's ID is unexpectedly zero")
			}
			if r.PublishedAt.IsZero() {
				t.Error("Release time is unexpectedly zero")
			}
			if r.RepoOwner != "rhysd-test" {
				t.Error("Repo owner should be rhysd-test:", r.RepoOwner)
			}
			if !strings.HasPrefix(r.RepoName, "test-release-") {
				t.Error("Repo name was not properly detectd:", r.RepoName)
			}
		})
	}
}

func TestDetectReleaseButNoAsset(t *testing.T) {
	_, ok, err := DetectLatest("rhysd/clever-f.vim")
	if err != nil {
		t.Fatal("Fetch failed:", err)
	}
	if ok {
		t.Fatal("When no asset found, result should be marked as 'not found'")
	}
}

func TestDetectNoRelease(t *testing.T) {
	_, ok, err := DetectLatest("rhysd/clever-f.vim")
	if err != nil {
		t.Fatal("Fetch failed:", err)
	}
	if ok {
		t.Fatal("When no release found, result should be marked as 'not found'")
	}
}

func TestInvalidSlug(t *testing.T) {
	up := DefaultUpdater()

	for _, slug := range []string{
		"foo",
		"/",
		"foo/",
		"/bar",
		"foo/bar/piyo",
	} {
		_, _, err := up.DetectLatest(slug)
		if err == nil {
			t.Error(slug, "should be invalid slug")
		}
		if !strings.Contains(err.Error(), "Invalid slug format") {
			t.Error("Unexpected error for", slug, ":", err)
		}
	}
}

func TestNonExistingRepo(t *testing.T) {
	v, ok, err := DetectLatest("rhysd/non-existing-repo")
	if err != nil {
		t.Fatal("Non-existing repo should not cause an error:", v)
	}
	if ok {
		t.Fatal("Release for non-existing repo should not be found")
	}
}

func TestNoReleaseFound(t *testing.T) {
	_, ok, err := DetectLatest("rhysd/misc")
	if err != nil {
		t.Fatal("Repo having no release should not cause an error:", err)
	}
	if ok {
		t.Fatal("Repo having no release should not be found")
	}
}

func TestDetectFromBrokenGitHubEnterpriseURL(t *testing.T) {
	up, err := NewUpdater(Config{APIToken: "hogehoge", EnterpriseBaseURL: "https://example.com"})
	if err != nil {
		t.Fatal(err)
	}
	_, ok, _ := up.DetectLatest("foo/bar")
	if ok {
		t.Fatal("Invalid GitHub Enterprise base URL should raise an error")
	}
}

func TestDetectFromGitHubEnterpriseRepo(t *testing.T) {
	token := os.Getenv("GITHUB_ENTERPRISE_TOKEN")
	base := os.Getenv("GITHUB_ENTERPRISE_BASE_URL")
	repo := os.Getenv("GITHUB_ENTERPRISE_REPO")
	if token == "" {
		t.Skip("because token for GHE is not found")
	}
	if base == "" {
		t.Skip("because base URL for GHE is not found")
	}
	if repo == "" {
		t.Skip("because repo slug for GHE is not found")
	}

	up, err := NewUpdater(Config{APIToken: token, EnterpriseBaseURL: base})
	if err != nil {
		t.Fatal(err)
	}

	r, ok, err := up.DetectLatest(repo)
	if err != nil {
		t.Fatal("Fetch failed:", err)
	}
	if !ok {
		t.Fatal(repo, "not found")
	}
	if r == nil {
		t.Fatal("Release not detected")
	}
	if !r.Version.Equals(semver.MustParse("1.2.3")) {
		t.Error("")
	}
}

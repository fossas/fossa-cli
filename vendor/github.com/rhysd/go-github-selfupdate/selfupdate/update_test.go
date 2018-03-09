package selfupdate

import (
	"github.com/blang/semver"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func setupTestBinary() {
	if err := exec.Command("go", "build", "./testdata/github-release-test/").Run(); err != nil {
		panic(err)
	}
}

func teardownTestBinary() {
	bin := "github-release-test"
	if runtime.GOOS == "windows" {
		bin = "github-release-test.exe"
	}
	if err := os.Remove(bin); err != nil {
		panic(err)
	}
}

func TestUpdateCommand(t *testing.T) {
	if testing.Short() {
		t.Skip("skip tests in short mode.")
	}

	for _, slug := range []string{
		"rhysd-test/test-release-zip",
		"rhysd-test/test-release-tar",
		"rhysd-test/test-release-gzip",
		"rhysd-test/test-release-tar-xz",
		"rhysd-test/test-release-xz",
		"rhysd-test/test-release-contain-version",
	} {
		t.Run(slug, func(t *testing.T) {
			setupTestBinary()
			defer teardownTestBinary()
			latest := semver.MustParse("1.2.3")
			prev := semver.MustParse("1.2.2")
			rel, err := UpdateCommand("github-release-test", prev, slug)
			if err != nil {
				t.Fatal(err)
			}
			if rel.Version.NE(latest) {
				t.Error("Version is not latest", rel.Version)
			}
			bytes, err := exec.Command(filepath.FromSlash("./github-release-test")).Output()
			if err != nil {
				t.Fatal("Failed to run test binary after update:", err)
			}
			out := string(bytes)
			if out != "v1.2.3\n" {
				t.Error("Output from test binary after update is unexpected:", out)
			}
		})
	}
}

func TestUpdateViaSymlink(t *testing.T) {
	if testing.Short() {
		t.Skip("skip tests in short mode.")
	}
	if runtime.GOOS == "windows" && os.Getenv("APPVEYOR") == "" {
		t.Skip("skipping because creating symlink on windows requires the root privilege")
	}

	setupTestBinary()
	defer teardownTestBinary()
	exePath := "github-release-test"
	symPath := "github-release-test-sym"
	if runtime.GOOS == "windows" {
		exePath = "github-release-test.exe"
		symPath = "github-release-test-sym.exe"
	}
	if err := os.Symlink(exePath, symPath); err != nil {
		t.Fatal(err)
	}
	defer os.Remove(symPath)

	latest := semver.MustParse("1.2.3")
	prev := semver.MustParse("1.2.2")
	rel, err := UpdateCommand(symPath, prev, "rhysd-test/test-release-zip")
	if err != nil {
		t.Fatal(err)
	}
	if rel.Version.NE(latest) {
		t.Error("Version is not latest", rel.Version)
	}

	// Test not symbolic link, but actual physical executable
	bytes, err := exec.Command(filepath.FromSlash("./github-release-test")).Output()
	if err != nil {
		t.Fatal("Failed to run test binary after update:", err)
	}
	out := string(bytes)
	if out != "v1.2.3\n" {
		t.Error("Output from test binary after update is unexpected:", out)
	}

	s, err := os.Lstat(symPath)
	if err != nil {
		t.Fatal(err)
	}
	if s.Mode()&os.ModeSymlink == 0 {
		t.Fatalf("%s is not a symlink.", symPath)
	}
	p, err := filepath.EvalSymlinks(symPath)
	if err != nil {
		t.Fatal(err)
	}
	if p != exePath {
		t.Fatal("Created symlink no loger points the executable:", p)
	}
}

func TestUpdateBrokenSymlinks(t *testing.T) {
	if runtime.GOOS == "windows" && os.Getenv("APPVEYOR") == "" {
		t.Skip("skipping because creating symlink on windows requires the root privilege")
	}

	// unknown-xxx -> unknown-yyy -> {not existing}
	xxx := "unknown-xxx"
	yyy := "unknown-yyy"
	if runtime.GOOS == "windows" {
		xxx = "unknown-xxx.exe"
		yyy = "unknown-yyy.exe"
	}
	if err := os.Symlink("not-existing", yyy); err != nil {
		t.Fatal(err)
	}
	defer os.Remove(yyy)
	if err := os.Symlink(yyy, xxx); err != nil {
		t.Fatal(err)
	}
	defer os.Remove(xxx)

	v := semver.MustParse("1.2.2")
	for _, p := range []string{yyy, xxx} {
		_, err := UpdateCommand(p, v, "owner/repo")
		if err == nil {
			t.Fatal("Error should occur for unlinked symlink", p)
		}
		if !strings.Contains(err.Error(), "Failed to resolve symlink") {
			t.Fatal("Unexpected error for broken symlink", p, err)
		}
	}
}

func TestNotExistingCommandPath(t *testing.T) {
	_, err := UpdateCommand("not-existing-command-path", semver.MustParse("1.2.2"), "owner/repo")
	if err == nil {
		t.Fatal("Not existing command path should cause an error")
	}
	if !strings.Contains(err.Error(), "File may not exist") {
		t.Fatal("Unexpected error for not existing command path", err)
	}
}

func TestNoReleaseFoundForUpdate(t *testing.T) {
	v := semver.MustParse("1.0.0")
	fake := filepath.FromSlash("./testdata/fake-executable")
	rel, err := UpdateCommand(fake, v, "rhysd/misc")
	if err != nil {
		t.Fatal("No release should not make an error:", err)
	}
	if rel.Version.NE(v) {
		t.Error("No release should return the current version as the latest:", rel.Version)
	}
	if rel.URL != "" {
		t.Error("Browse URL should be empty when no release found:", rel.URL)
	}
	if rel.AssetURL != "" {
		t.Error("Asset URL should be empty when no release found:", rel.AssetURL)
	}
	if rel.ReleaseNotes != "" {
		t.Error("Release notes should be empty when no release found:", rel.ReleaseNotes)
	}
}

func TestCurrentIsTheLatest(t *testing.T) {
	if testing.Short() {
		t.Skip("skip tests in short mode.")
	}
	setupTestBinary()
	defer teardownTestBinary()

	v := semver.MustParse("1.2.3")
	rel, err := UpdateCommand("github-release-test", v, "rhysd-test/test-release-zip")
	if err != nil {
		t.Fatal(err)
	}
	if rel.Version.NE(v) {
		t.Error("v1.2.3 should be the latest:", rel.Version)
	}
	if rel.URL == "" {
		t.Error("Browse URL should not be empty when release found:", rel.URL)
	}
	if rel.AssetURL == "" {
		t.Error("Asset URL should not be empty when release found:", rel.AssetURL)
	}
	if rel.ReleaseNotes == "" {
		t.Error("Release notes should not be empty when release found:", rel.ReleaseNotes)
	}
}

func TestBrokenBinaryUpdate(t *testing.T) {
	if testing.Short() {
		t.Skip("skip tests in short mode.")
	}

	fake := filepath.FromSlash("./testdata/fake-executable")
	_, err := UpdateCommand(fake, semver.MustParse("1.2.2"), "rhysd-test/test-incorrect-release")
	if err == nil {
		t.Fatal("Error should occur for broken package")
	}
	if !strings.Contains(err.Error(), "Failed to uncompress .tar.gz file") {
		t.Fatal("Unexpected error:", err)
	}
}

func TestInvalidSlugForUpdate(t *testing.T) {
	fake := filepath.FromSlash("./testdata/fake-executable")
	_, err := UpdateCommand(fake, semver.MustParse("1.0.0"), "rhysd/")
	if err == nil {
		t.Fatal("Unknown repo should cause an error")
	}
	if !strings.Contains(err.Error(), "Invalid slug format") {
		t.Fatal("Unexpected error:", err)
	}
}

func TestInvalidAssetURL(t *testing.T) {
	err := UpdateTo("https://github.com/rhysd/non-existing-repo/releases/download/v1.2.3/foo.zip", "foo")
	if err == nil {
		t.Fatal("Error should occur for URL not found")
	}
	if !strings.Contains(err.Error(), "Failed to download a release file") {
		t.Fatal("Unexpected error:", err)
	}
}

func TestBrokenAsset(t *testing.T) {
	asset := "https://github.com/rhysd-test/test-incorrect-release/releases/download/invalid/broken-zip.zip"
	err := UpdateTo(asset, "foo")
	if err == nil {
		t.Fatal("Error should occur for URL not found")
	}
	if !strings.Contains(err.Error(), "Failed to uncompress zip file") {
		t.Fatal("Unexpected error:", err)
	}
}

func TestBrokenGitHubEnterpriseURL(t *testing.T) {
	up, err := NewUpdater(Config{APIToken: "hogehoge", EnterpriseBaseURL: "https://example.com"})
	if err != nil {
		t.Fatal(err)
	}
	err = up.UpdateTo(&Release{AssetURL: "https://example.com"}, "foo")
	if err == nil {
		t.Fatal("Invalid GitHub Enterprise base URL should raise an error")
	}
	if !strings.Contains(err.Error(), "Failed to call GitHub Releases API for getting an asset") {
		t.Error("Unexpected error occurred:", err)
	}
}

func TestUpdateFromGitHubEnterprise(t *testing.T) {
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

	setupTestBinary()
	defer teardownTestBinary()

	up, err := NewUpdater(Config{APIToken: token, EnterpriseBaseURL: base})
	if err != nil {
		t.Fatal(err)
	}

	latest := semver.MustParse("1.2.3")
	prev := semver.MustParse("1.2.2")
	rel, err := up.UpdateCommand("github-release-test", prev, repo)
	if err != nil {
		t.Fatal(err)
	}

	if rel.Version.NE(latest) {
		t.Error("Version is not latest", rel.Version)
	}

	bytes, err := exec.Command(filepath.FromSlash("./github-release-test")).Output()
	if err != nil {
		t.Fatal("Failed to run test binary after update:", err)
	}

	out := string(bytes)
	if out != "v1.2.3\n" {
		t.Error("Output from test binary after update is unexpected:", out)
	}
}

func TestUpdateFromGitHubPrivateRepo(t *testing.T) {
	token := os.Getenv("GITHUB_PRIVATE_TOKEN")
	if token == "" {
		t.Skip("because GITHUB_PRIVATE_TOKEN is not set")
	}

	setupTestBinary()
	defer teardownTestBinary()

	up, err := NewUpdater(Config{APIToken: token})
	if err != nil {
		t.Fatal(err)
	}

	latest := semver.MustParse("1.2.3")
	prev := semver.MustParse("1.2.2")
	rel, err := up.UpdateCommand("github-release-test", prev, "rhysd/private-release-test")
	if err != nil {
		t.Fatal(err)
	}

	if rel.Version.NE(latest) {
		t.Error("Version is not latest", rel.Version)
	}

	bytes, err := exec.Command(filepath.FromSlash("./github-release-test")).Output()
	if err != nil {
		t.Fatal("Failed to run test binary after update:", err)
	}

	out := string(bytes)
	if out != "v1.2.3\n" {
		t.Error("Output from test binary after update is unexpected:", out)
	}
}

package selfupdate

import (
	"os"
	"testing"
)

func TestGitHubTokenEnv(t *testing.T) {
	token := os.Getenv("GITHUB_TOKEN")
	if token == "" {
		t.Skip("because $GITHUB_TOKEN is not set")
	}
	_ = DefaultUpdater()
	if _, err := NewUpdater(Config{}); err != nil {
		t.Error("Failed to initialize updater with empty config")
	}
	if _, err := NewUpdater(Config{APIToken: token}); err != nil {
		t.Error("Failed to initialize updater with API token config")
	}
}

func TestGitHubTokenIsNotSet(t *testing.T) {
	token := os.Getenv("GITHUB_TOKEN")
	if token != "" {
		defer os.Setenv("GITHUB_TOKEN", token)
	}
	os.Setenv("GITHUB_TOKEN", "")
	_ = DefaultUpdater()
	if _, err := NewUpdater(Config{}); err != nil {
		t.Error("Failed to initialize updater with empty config")
	}
}

func TestGitHubEnterpriseClient(t *testing.T) {
	url := "https://github.company.com/api/v3/"
	up, err := NewUpdater(Config{APIToken: "hogehoge", EnterpriseBaseURL: url})
	if err != nil {
		t.Fatal(err)
	}
	if up.api.BaseURL.String() != url {
		t.Error("Base URL was set to", up.api.BaseURL, ", want", url)
	}
	if up.api.UploadURL.String() != url {
		t.Error("Upload URL was set to", up.api.UploadURL, ", want", url)
	}

	url2 := "https://upload.github.company.com/api/v3/"
	up, err = NewUpdater(Config{
		APIToken:            "hogehoge",
		EnterpriseBaseURL:   url,
		EnterpriseUploadURL: url2,
	})
	if err != nil {
		t.Fatal(err)
	}
	if up.api.BaseURL.String() != url {
		t.Error("Base URL was set to", up.api.BaseURL, ", want", url)
	}
	if up.api.UploadURL.String() != url2 {
		t.Error("Upload URL was set to", up.api.UploadURL, ", want", url2)
	}
}

func TestGitHubEnterpriseClientWithoutToken(t *testing.T) {
	token := os.Getenv("GITHUB_TOKEN")
	defer os.Setenv("GITHUB_TOKEN", token)
	os.Setenv("GITHUB_TOKEN", "")
	_, err := NewUpdater(Config{EnterpriseBaseURL: "https://github.company.com/api/v3/"})
	if err == nil {
		t.Fatal("Error should be reported because of empty token")
	}
}

func TestGitHubEnterpriseClientInvalidURL(t *testing.T) {
	_, err := NewUpdater(Config{APIToken: "hogehoge", EnterpriseBaseURL: ":this is not a URL"})
	if err == nil {
		t.Fatal("Invalid URL should raise an error")
	}
}

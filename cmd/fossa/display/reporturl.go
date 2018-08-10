package display

import (
	"net/url"
	"strings"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/config"
)

func ReportURL(locator fossa.Locator) string {
	baseURL, err := url.Parse(config.Endpoint())
	if err != nil {
		log.Fatalf("Invalid FOSSA endpoint: %s", err.Error())
	}
	reportBranch := config.Branch()
	if reportBranch == "" {
		reportBranch = "master"
	}
	reportURL, err := url.Parse(
		"/projects/" +
			url.PathEscape(locator.Fetcher+"+"+locator.Project) +
			"/refs/branch/" +
			url.PathEscape(reportBranch) +
			"/" +
			url.PathEscape(locator.Revision))
	return `
============================================================

    View FOSSA Report:
    ` + strings.Replace(baseURL.ResolveReference(reportURL).String(), "%", "%%", -1) + `

============================================================
`
}

package cmdutil

import (
	"fmt"
	"net/url"
	"strings"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
)

func FmtReportURL(locator fossa.Locator) string {
	baseURL, err := url.Parse(config.Endpoint())
	if err != nil {
		log.Logger.Fatalf("Invalid FOSSA endpoint: %s", err.Error())
	}
	reportBranch := config.Branch()
	if reportBranch == "" {
		reportBranch = "master"
	}
	reportURL, err := url.Parse(
		"/projects/" +
			url.QueryEscape(locator.Fetcher+"+"+locator.Project) +
			"/refs/branch/" +
			reportBranch +
			"/" +
			url.QueryEscape(locator.Revision) +
			"/browse/dependencies")
	return fmt.Sprintf(`
============================================================

    View FOSSA Report:
    ` + strings.Replace(baseURL.ResolveReference(reportURL).String(), "%", "%%", -1) + `

============================================================
`)
}

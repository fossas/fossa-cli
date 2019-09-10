package fossa

import (
	"fmt"
	"net/url"

	"github.com/fossas/fossa-cli/errors"
)

const BuildsAPI = "/api/cli/%s/latest_build"

// A Build holds the FOSSA API response for the builds API.
type Build struct {
	ID    int
	Error string
	Task  struct {
		Status string
	}
}

// GetLatestBuild loads the most recent build for a revision
// or returns an error if the revision does not exist, or the revision has no builds.
func GetLatestBuild(locator Locator) (Build, *errors.Error) {
	var build Build
	statusCode, err := GetJSON(fmt.Sprintf(BuildsAPI, url.PathEscape(locator.OrgString())), &build)
	if err != nil {
		return Build{}, &errors.Error{
			ExitCode:        statusCode,
			Cause:           err,
			Type:            errors.Unknown,
			Troubleshooting: fmt.Sprintf("Could not get latest build for project `%s` revision: `%s` from API. Ensure that you have already analyzed your project by checking to see if it exists on fossa.com.", locator.Project, locator.Revision),
		}
	}

	return build, nil
}

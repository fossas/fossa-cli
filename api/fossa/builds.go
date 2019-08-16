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
func GetLatestBuild(locator Locator) (Build, int, error) {
	var build Build
	status_code, err := GetJSON(fmt.Sprintf(BuildsAPI, url.PathEscape(locator.OrgString())), &build)
	if err != nil {
		return Build{}, status_code, errors.Wrap(err, "could not get Build from API")
	}

	return build, status_code, nil
}

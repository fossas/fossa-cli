package fossa

import (
	"fmt"
	"net/url"

	"github.com/pkg/errors"
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

// GetLatestBuild loads the most recent build for a revision.
func GetLatestBuild(locator Locator) (Build, error) {
	var build Build
	_, err := GetJSON(fmt.Sprintf(BuildsAPI, url.PathEscape(locator.String())), &build)
	if err != nil {
		return Build{}, errors.Wrap(err, "could not get Build from API")
	}

	return build, nil
}

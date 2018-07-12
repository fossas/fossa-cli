package fossa

import (
	"fmt"
	"net/url"

	"github.com/pkg/errors"
)

const BuildRevisionAPI = "/api/revisions/%s/build"
const BuildsAPI = "/api/builds/%s"

// A Build holds the FOSSA API response for the builds API.
type Build struct {
	ID    int
	Error string
	Task  struct {
		Status string
	}
}

func QueueBuild(locator Locator) (Build, error) {
	q := url.Values{}
	q.Add("locator", locator.String())

	var build Build
	_, err := GetJSON(fmt.Sprintf(BuildsAPI, "?"+q.Encode()), &build)
	if err != nil {
		return Build{}, errors.Wrap(err, "could not get Build from API")
	}

	return build, nil
}

// GetBuilds loads the build for a revision.
func GetBuilds(locator Locator) ([]Build, error) {
	q := url.Values{}
	q.Add("locator", locator.String())
	q.Add("sort", "-createdAt")

	var builds []Build
	_, err := GetJSON(fmt.Sprintf(BuildsAPI, "?"+q.Encode()), &builds)
	if err != nil {
		return nil, errors.Wrap(err, "could not get Build from API")
	}

	return builds, nil
}

package fossa

import (
	"fmt"
	"net/url"

	"github.com/pkg/errors"
)

const IssuesAPI = "/api/issues/%s"

// An Issue holds the FOSSA API response for the issue API.
type Issue struct {
	PriorityString string
	Resolved       bool
	Revision       Revision
	Type           string
}

// GetIssues loads the issues for a project.
func GetIssues(locator Locator) ([]Issue, error) {
	q := url.Values{}
	q.Add("fromRevision", locator.String())
	q.Add("count", "1000")
	var issues []Issue
	_, err := GetJSON(fmt.Sprintf(IssuesAPI, "?"+q.Encode()), &issues)
	if err != nil {
		return nil, errors.Wrap(err, "could not get Issues from API")
	}

	return issues, nil
}

package fossa

import (
	"fmt"
	"net/url"

	"github.com/pkg/errors"
)

const IssuesAPI = "/api/cli/%s/issues"

// An Issue holds the FOSSA API response for the issue API.
type Issue struct {
	ID             int
	PriorityString string
	Resolved       bool
	Revision       Revision
	Type           string
}

// A wrapped list of issues returned by the FOSSA CLI issues endpoint
// If a push-only API key is used, then only the count is returned
type Issues struct {
	Count  int
	Issues []Issue
}

// GetIssues loads the issues for a project.
func GetIssues(locator Locator) (Issues, error) {
	var issues Issues
	_, err := GetJSON(fmt.Sprintf(IssuesAPI, url.PathEscape(locator.OrgString())), &issues)
	if err != nil {
		return Issues{}, errors.Wrap(err, "could not get Issues from API")
	}

	return issues, nil
}

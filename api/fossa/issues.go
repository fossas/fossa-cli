package fossa

import (
	"fmt"
	"net/url"
	"regexp"

	"github.com/fossas/fossa-cli/errors"
)

const IssuesAPI = "/api/cli/%s/issues"

// A wrapped list of issues returned by the FOSSA CLI issues endpoint
// If a push-only API key is used, then only the count is returned
type Issues struct {
	Count  int
	Issues []Issue
	Status string

	NormalizedByType map[string][]Issue
}

// An Issue holds the FOSSA API response for the issue API.
type Issue struct {
	ID             int    `json:"id"`
	PriorityString string `json:"priorityString"`
	Resolved       bool   `json:"resolved"`
	RevisionID     string `json:"revisionId"`
	Type           string `json:"type"`
	Rule           Rule   `json:"rule"`

	Name     string
	Revision string
}

// Rule holds the representation of an Issue's Rule.
type Rule struct {
	License string `json:"licenseId"`
}

// GetIssues loads the issues for a project.
func GetIssues(locator Locator) (Issues, error) {
	var issues Issues
	_, err := GetJSON(fmt.Sprintf(IssuesAPI, url.PathEscape(locator.OrgString())), &issues)
	if err != nil {
		return Issues{}, errors.Wrap(err, "could not get Issues from API")
	}

	issues.normalize()
	return issues, nil
}

func (issues *Issues) normalize() {
	typeMap := make(map[string][]Issue)
	for _, issue := range issues.Issues {
		issue.extractLocator()
		formattedType := formatType(issue.Type)
		typeMap[formattedType] = append(typeMap[formattedType], issue)
	}

	issues.NormalizedByType = typeMap
}

func formatType(issueType string) string {
	switch issueType {
	case "policy_conflict":
		return "Denied by Policy"
	case "policy_flag":
		return "Flagged by Policy"
	case "vulnerability":
		return "Vulnerability"
	case "unlicensed_dependency":
		return "Unlicensed Dependency"
	case "outdated_dependency":
		return "Outdated Dependency"
	default:
		return issueType
	}
}

func (issue *Issue) extractLocator() {
	locator := regexp.MustCompile("[+$]").Split(issue.RevisionID, -1)
	if len(locator) >= 2 {
		issue.Name = locator[1]
	}
	if len(locator) >= 3 {
		issue.Revision = locator[2]
	}
}

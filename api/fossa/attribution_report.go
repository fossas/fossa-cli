package fossa

import (
	"fmt"
	"net/url"
)

// AttributionDependencyLicense is the structure of a license in an attribution report
type AttributionDependencyLicense struct {
	Name        string
	Attribution string
}

// AttributionReportRow is the structure of a single dependency in an attribution report
type AttributionReportRow struct {
	Package         string
	Source          string
	Version         string
	IsGolang        bool
	Hash            string
	Authors         []string
	Description     string
	Licenses        []AttributionDependencyLicense
	OtherLicenses   []AttributionDependencyLicense
	ProjectURL      string
	DependencyPaths []string
	Notes           []string
	DownloadURL     string
}

// AttributionReportProject is the project for which the report is being made
type AttributionReportProject struct {
	Name     string
	Revision string
}

// AttributionReport is the report including the dependency and license data for a project
type AttributionReport struct {
	Project            AttributionReportProject
	DirectDependencies []AttributionReportRow
	DeepDependencies   []AttributionReportRow
	Licenses           map[string]string
}

const reportAPI = "/api/revisions/%s/attribution/json"

// GetReport gets a report for the provided locator
func GetReport(locator Locator) (AttributionReport, error) {
	var report AttributionReport
	params := url.Values{}

	params.Add("includeDeepDependencies", "true")
	params.Add("includeHashAndVersionData", "true")
	params.Add("includeDownloadUrl", "true")

	url := fmt.Sprintf(reportAPI, url.PathEscape(locator.OrgString())) + "?" + params.Encode()

	_, err := GetJSON(url, &report)
	return report, err
}

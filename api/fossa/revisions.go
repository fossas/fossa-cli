package fossa

import (
	"fmt"
	"net/url"

	"github.com/apex/log"
)

// A License holds the FOSSA API response for the license API.
type License struct {
	ID             int64
	LicenseID      string
	RevisionID     string
	LicenseGroupID int64
	Ignored        bool
	Title          string
	URL            string
	FullText       string
	Copyright      string
}

// A Revision holds the FOSSA API response for the revision API.
type Revision struct {
	Locator  *Locator `json:"loc"`
	Licenses []License
	Project  *Project
	Meta     []RevisionMeta
	Issues   []Issue
}

// A RevisionMeta holds metadata about a FOSSA API revision.
type RevisionMeta struct {
	LastScan string `json:"last_scan"`
}

// A Project holds the FOSSA API response for the project API.
type Project struct {
	Title   string
	URL     string
	Public  bool
	Authors []string
}

// RevisionsAPI is the API endpoint for revisions.
const RevisionsAPI = "/api/revisions/%s"

// GetRevision loads a single revision.
func GetRevision(locator Locator) (Revision, error) {
	var revision Revision
	_, err := GetJSON(fmt.Sprintf(RevisionsAPI, url.PathEscape(locator.String())), &revision)
	if err != nil {
		return Revision{}, err
	}

	if revision.Locator == nil {
		revision.Locator = &locator
	}
	if len(revision.Licenses) == 0 {
		revision.Licenses = append(revision.Licenses, License{
			LicenseID: "UNKNOWN",
		})
	}
	if revision.Project == nil {
		revision.Project = &Project{
			Title: revision.Locator.Project,
			URL:   "UNKNOWN",
		}
	}

	return revision, err
}

// GetRevisions loads many revisions in batched requests.
func GetRevisions(locators []Locator) (revs []Revision, err error) {
	var locs []string
	for _, loc := range locators {
		locs = append(locs, loc.String())
	}

	// Split locators into chunks of 20 (this is an API limitation).
	chunks := make([][]string, 0)
	chunkSize := 20
	for i := 0; i < len(locs); i += chunkSize {
		end := i + chunkSize

		if end > len(locs) {
			end = len(locs)
		}

		chunks = append(chunks, locs[i:end])
	}

	// Make chunked API calls in parallel.
	responses := make(chan []Revision, len(chunks))
	for _, chunk := range chunks {
		qs := url.Values{}
		for _, q := range chunk {
			qs.Add("locator", q)
		}

		go func(endpoint string) {
			var revisions []Revision
			_, err := GetJSON(endpoint, &revisions)
			if err != nil {
				log.Logger.Warningf("Failed to get some revisions: %s", err.Error())
				responses <- []Revision{}
			} else {
				responses <- revisions
			}
		}(fmt.Sprintf(RevisionsAPI, "?"+qs.Encode()))
	}

	var revisions []Revision
	for range chunks {
		select {
		case r := <-responses:
			revisions = append(revisions, r...)
		}
	}

	return revisions, err
}

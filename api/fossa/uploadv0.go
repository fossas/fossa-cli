package fossa

import (
	"encoding/json"
	"net/url"

	"github.com/apex/log"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/cmd/fossa/version"
)

// UploadOptions are optional keys that provide extra metadata for an upload.
type UploadOptions struct {
	Branch         string
	ProjectURL     string
	JIRAProjectKey string
	Link           string
	Team           string
}

// Upload uploads a project's analysis.
func UploadV0(title string, locator Locator, options UploadOptions, data []SourceUnit) (Locator, error) {
	log.Debug("Uploading build using API v0")
	// Check preconditions.
	if locator.Fetcher == "git" && locator.Revision == "" {
		return Locator{}, ErrRevisionInvalid
	}
	if locator.Project == "" {
		return Locator{}, ErrProjectIdInvalid
	}
	if len(data) == 0 {
		return Locator{}, ErrEmptyDataUpload
	}

	payload, err := json.Marshal(data)
	if err != nil {
		return Locator{}, errors.Wrap(err, "could not marshal upload data")
	}
	log.WithFields(log.Fields{
		"modules": data,
		"payload": string(payload),
	}).Debug("uploading build")

	q := url.Values{}
	q.Add("locator", locator.String())
	q.Add("v", version.ShortString())
	if locator.Fetcher == "custom" {
		q.Add("managedBuild", "true")
		q.Add("title", title)
	}

	if options.Branch != "" {
		q.Add("branch", options.Branch)
	}
	if options.ProjectURL != "" {
		q.Add("projectURL", options.ProjectURL)
	}
	if options.JIRAProjectKey != "" {
		q.Add("jiraProjectKey", options.JIRAProjectKey)
	}
	if options.Link != "" {
		q.Add("link", options.Link)
	}
	if options.Team != "" {
		q.Add("team", options.Team)
	}

	endpoint, err := url.Parse("/api/builds/custom?" + q.Encode())
	if err != nil {
		return Locator{}, errors.New("Failed to generate upload URL")
	}
	log.WithField("endpoint", endpoint.String()).Debug("uploading build")

	res, statusCode, err := Post(endpoint.String(), payload)
	log.WithField("response", res).Debug("build upload completed")

	if statusCode == 428 {
		// TODO: handle "Managed Project" workflow
		return Locator{}, errors.New("invalid project or revision; make sure this version is published and FOSSA has access to your repo (to submit a custom project, set Fetcher to `custom` in `.fossa.yml`)")
	} else if statusCode == 403 {
		return Locator{}, ErrForbidden
	} else if err != nil {
		return Locator{}, errors.Wrap(err, "could not upload")
	}
	log.Debug("build upload succeeded")

	var unmarshalled struct {
		Locator string
		Error   string
	}
	err = json.Unmarshal([]byte(res), &unmarshalled)
	if err != nil || unmarshalled.Error != "" {
		return Locator{}, errors.Errorf("Error response from API: %s", res)
	}

	if unmarshalled.Locator == "" {
		return Locator{}, errors.Errorf("bad response: %s", res)
	}

	return ReadLocator(unmarshalled.Locator), nil
}

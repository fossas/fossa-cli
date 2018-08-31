package fossa

import (
	"encoding/json"
	"net/url"

	"github.com/pkg/errors"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/cmd/fossa/version"
)

// Errors related to preconditions.
var (
	ErrNoProject   = errors.New("no project provided for upload")
	ErrNoRevision  = errors.New("no revision provided for upload")
	ErrNoBuildData = errors.New("no build data to upload")
)

// Errors resulting from a bad API response.
var (
	ErrForbidden            = errors.New("authentication failed (is the API key correct?)")
	ErrRevisionDoesNotExist = errors.New("revision does not exist (are the project and revision correct and published in FOSSA?)")
)

// UploadOptions are optional keys that provide extra metadata for an upload.
type UploadOptions struct {
	Branch         string
	ProjectURL     string
	JIRAProjectKey string
}

// Upload uploads a project's analysis.
func Upload(title string, locator Locator, options UploadOptions, data []SourceUnit) (Locator, error) {
	// Check preconditions.
	if locator.Fetcher != "custom" && locator.Revision == "" {
		return Locator{}, errors.New("Could not infer revision name from `git` remote named `origin`. To submit a custom project, set Fetcher to `custom` in `.fossa.yml`")
	}
	if locator.Project == "" {
		return Locator{}, errors.New("Could not infer project name from either `.fossa.yml` or `git` remote named `origin`")
	}
	if len(data) == 0 {
		return Locator{}, errors.New("No data to upload")
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
		return Locator{}, errors.New("you do not have permission to upload builds for this project")
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

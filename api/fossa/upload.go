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

// Upload uploads a project's analysis.
func Upload(fetcher, project, revision, title, branch string, data []SourceUnit) (Locator, error) {
	// Check preconditions
	if fetcher != "custom" && revision == "" {
		log.Fatal("Could not infer revision name from `git` remote named `origin`. To submit a custom project, set Fetcher to `custom` in `.fossa.yml`")
	}
	if project == "" {
		log.Fatal("Could not infer project name from either `.fossa.yml` or `git` remote named `origin`")
	}
	if len(data) == 0 {
		log.Fatal("No data to upload")
	}

	payload, err := json.Marshal(data)
	if err != nil {
		return Locator{}, errors.Wrap(err, "could not marshal upload data")
	}
	log.WithFields(log.Fields{
		"modules": data,
		"payload": string(payload),
	}).Debug("uploading build")

	locator := Locator{Fetcher: fetcher, Project: project, Revision: revision}

	q := url.Values{}
	q.Add("locator", locator.String())
	q.Add("v", version.ShortString())

	if fetcher == "custom" {
		q.Add("managedBuild", "true")
		q.Add("title", title)
	}
	if branch != "" {
		q.Add("branch", branch)
	}
	if revision != "" {
		q.Add("revision", revision)
	}
	endpoint, err := url.Parse("/api/builds/custom?" + q.Encode())
	if err != nil {
		log.Fatal("Failed to generate upload URL")
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

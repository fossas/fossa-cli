package fossa

import (
	"encoding/json"
	"net/url"

	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/cmd/fossa/version"
	"github.com/fossas/fossa-cli/log"
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

// func UploadGitProject(project, revision string, data []SourceUnit) {}

// func UploadCustomProject(project, revision, title string, data []SourceUnit) {}

func Upload(fetcher, project, revision, title, branch string, data []SourceUnit) (Locator, error) {
	// Check preconditions
	if fetcher != "custom" && revision == "" {
		log.Logger.Fatalf("Could not infer revision name from `git` remote named `origin`. To submit a custom project, set Fetcher to `custom` in `.fossa.yml`")
	}
	if project == "" {
		log.Logger.Fatalf("Could not infer project name from either `.fossa.yml` or `git` remote named `origin`")
	}
	if len(data) == 0 {
		log.Logger.Fatalf("No data to upload")
	}

	payload, err := json.Marshal(data)
	if err != nil {
		return Locator{}, errors.Wrap(err, "could not marshal upload data")
	}
	log.Logger.Debugf("Uploading data from %#v modules: %#v", len(data), string(payload))

	locator := Locator{Fetcher: fetcher, Project: project, Revision: revision}

	var ep *url.URL
	q := url.Values{}
	if ep, err = url.Parse("/api/builds/custom"); err != nil {
		log.Logger.Fatal("Failed to generate upload uri")
	}
	ep.ForceQuery = true
	q.Add("locator", locator.QueryString())
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
	if ep, err = ep.Parse("?" + q.Encode()); err != nil {
		log.Logger.Fatal("Failed to generate upload uri")
	}
	log.Logger.Debugf("Sending build data to %#v", ep.String())

	res, statusCode, err := Post(ep.String(), payload)
	log.Logger.Debugf("Response: %#v", res)

	if statusCode == 428 {
		// TODO: handle "Managed Project" workflow
		return Locator{}, errors.New("invalid project or revision; make sure this version is published and FOSSA has access to your repo (to submit a custom project, set Fetcher to `custom` in `.fossa.yml`)")
	} else if statusCode == 403 {
		return Locator{}, errors.New("you do not have permission to upload builds for this project")
	} else if err != nil {
		return Locator{}, errors.Wrap(err, "could not upload")
	}
	log.Logger.Debugf("Upload finished")

	var unmarshalled struct {
		Locator string
	}
	err = json.Unmarshal([]byte(res), &unmarshalled)
	if err != nil {
		return Locator{}, errors.Errorf("bad response: %s", res)
	}

	return ReadLocator(unmarshalled.Locator), nil
}

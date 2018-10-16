package fossa

import (
	"encoding/json"

	"github.com/apex/log"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/cmd/fossa/version"
)

type V1UploadBody struct {
	VCS struct {
		Type      string
		Reference string
	}
	Project struct {
		Title string
		Id    string
		Url   string
		Jira  string
		Team  string
	}
	Revision struct {
		Id   string
		Link string
	}
	Meta struct {
		v string
	}
	Analysis []SourceUnit
}

// Upload uploads a project's analysis.
func UploadV1(uploadBody V1UploadBody) (Locator, error) {
	log.Debug("Uploading build using API v1")
	// Check preconditions.
	if uploadBody.Revision.Id == "" {
		return Locator{}, errors.New("Could not infer revision name from `git` remote named `origin`. To submit a custom project, set Fetcher to `custom` in `.fossa.yml`")
	}
	if uploadBody.Project.Id == "" {
		return Locator{}, errors.New("Could not infer project name from either `.fossa.yml` or `git` remote named `origin`")
	}
	if len(uploadBody.Analysis) == 0 {
		return Locator{}, errors.New("No data to upload")
	}

	payload, err := json.Marshal(uploadBody)
	if err != nil {
		return Locator{}, errors.Wrap(err, "could not marshal upload data")
	}
	log.WithFields(log.Fields{
		"modules": uploadBody.Analysis,
		"payload": string(payload),
	}).Debug("uploading build")

	uploadBody.Meta.v = version.ShortString()

	res, statusCode, err := Post("/api/cli/v1/build", payload)
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

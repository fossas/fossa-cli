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
		ID    string
		URL   string
		JIRA  string
		Team  string
	}
	Revision struct {
		ID   string
		Link string
	}
	Meta struct {
		V string
	}
	Analysis []SourceUnit
}

// Upload uploads a project's analysis.
func UploadV1(uploadBody V1UploadBody) (Locator, error) {
	log.Debug("Uploading build using API v1")
	// Check preconditions.
	if uploadBody.Revision.ID == "" {
		return Locator{}, ErrRevisionInvalid
	}
	if uploadBody.Project.ID == "" {
		return Locator{}, ErrProjectIdInvalid
	}
	if len(uploadBody.Analysis) == 0 {
		return Locator{}, ErrEmptyDataUpload
	}

	payload, err := json.Marshal(uploadBody)
	if err != nil {
		return Locator{}, errors.Wrap(err, "could not marshal upload data")
	}
	log.WithFields(log.Fields{
		"modules": uploadBody.Analysis,
		"payload": string(payload),
	}).Debug("uploading build")

	uploadBody.Meta.V = version.ShortString()

	res, statusCode, err := Post("/api/cli/v1/build", payload)
	log.WithField("response", res).Debug("build upload completed")

	if statusCode == 403 {
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
	if err != nil {
		return Locator{}, errors.Errorf("Could not unmarshal response from API: %s", res)
	}
	if unmarshalled.Error != "" {
		return Locator{}, errors.Errorf("API return an error on upload: %s", unmarshalled.Error)
	}

	if unmarshalled.Locator == "" {
		return Locator{}, errors.Errorf("bad response: %s", res)
	}

	return ReadLocator(unmarshalled.Locator), nil
}

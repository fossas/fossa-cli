package fossa

import (
	"errors"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/config"
)

var (
	ErrForbidden        = errors.New("you do not have permission to upload builds for this project (is the API key correct?)")
	ErrEmptyDataUpload  = errors.New("No data to upload")
	ErrRevisionInvalid  = errors.New("Could not infer revision name from `git` remote named `origin`. To submit a custom project, set Fetcher to `custom` in `.fossa.yml`")
	ErrProjectIdInvalid = errors.New("Could not infer project name from either `.fossa.yml` or `git` remote named `origin`")
)

func UploadAnalysis(normalized []SourceUnit) (Locator, error) {
	latestSupportedVersion, err := GetLatestSupportedAPIVersion()
	if err != nil {
		return Locator{}, err
	}

	switch latestSupportedVersion {
	case "v1":
		return uploadAnalysisV1(normalized)
	case "v0":
		fallthrough
	default:
		if latestSupportedVersion == "" {
			log.Warn("Could not find any compatible and supported API versions. Attempting to use the v0 upload path.")
		}

		return uploadAnalysisV0(normalized)
	}
}

func uploadAnalysisV1(normalized []SourceUnit) (Locator, error) {
	uploadBody := V1UploadBody{
		Analysis: normalized,
	}

	uploadBody.Project.ID = config.Project()
	uploadBody.Project.Team = config.Team()
	uploadBody.Project.URL = config.ProjectURL()
	uploadBody.Project.Title = config.Title()
	uploadBody.Project.JIRA = config.JIRAProjectKey()

	uploadBody.Revision.Link = config.Link()
	uploadBody.Revision.ID = config.Revision()

	uploadBody.VCS.Reference = config.Branch()

	if config.VCS() != nil {
		uploadBody.VCS.Type = "git"
	} else {
		uploadBody.VCS.Type = "none"
	}

	locator, err := UploadV1(uploadBody)
	display.ClearProgress()
	if err != nil {
		log.Fatalf("Error during upload: %s", err.Error())
	}
	return locator, nil
}

func uploadAnalysisV0(normalized []SourceUnit) (Locator, error) {
	locator, err := UploadV0(
		config.Title(),
		Locator{
			Fetcher:  config.Fetcher(),
			Project:  config.Project(),
			Revision: config.Revision(),
		},
		UploadOptions{
			Branch:         config.Branch(),
			ProjectURL:     config.ProjectURL(),
			JIRAProjectKey: config.JIRAProjectKey(),
			Link:           config.Link(),
			Team:           config.Team(),
		},
		normalized)
	display.ClearProgress()
	if err != nil {
		log.Fatalf("Error during upload: %s", err.Error())
		return Locator{}, err
	}
	return locator, nil
}

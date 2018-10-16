package fossa

import (
	"fmt"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/config"
)

func UploadAnalysis(normalized []SourceUnit) error {
	latestSupportedVersion, err := GetLatestSupportedAPIVersion()
	if err != nil {
		return err
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

func uploadAnalysisV1(normalized []SourceUnit) error {
	display.InProgress("Uploading analysis...")

	uploadBody := V1UploadBody{
		Analysis: normalized,
	}

	uploadBody.Project.Id = config.Project()
	uploadBody.Project.Team = config.Team()
	uploadBody.Project.Url = config.ProjectURL()
	uploadBody.Project.Title = config.Title()
	uploadBody.Project.Jira = config.JIRAProjectKey()

	uploadBody.Revision.Link = config.Link()
	uploadBody.Revision.Id = config.Revision()

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
	fmt.Println(locator.ReportURL())
	return nil
}

func uploadAnalysisV0(normalized []SourceUnit) error {
	display.InProgress("Uploading analysis...")
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
		return err
	}
	fmt.Println(locator.ReportURL())
	return nil
}

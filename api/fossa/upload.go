package fossa

import (
	"fmt"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/api/fossa/v0"
	"github.com/fossas/fossa-cli/api/fossa/v1"
	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/config"
)

func UploadAnalysis(normalized []fossa.SourceUnit) error {
	latestSupportedVersion, err := fossa.GetLatestSupportedAPIVersion()
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

func uploadAnalysisV1(normalized []fossa.SourceUnit) error {
	display.InProgress("Uploading analysis...")

	uploadBody := v1.V1UploadBody{
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

	locator, err := v1.Upload(uploadBody)
	display.ClearProgress()
	if err != nil {
		log.Fatalf("Error during upload: %s", err.Error())
	}
	fmt.Println(locator.ReportURL())
	return nil
}

func uploadAnalysisV0(normalized []fossa.SourceUnit) error {
	display.InProgress("Uploading analysis...")
	locator, err := v0.Upload(
		config.Title(),
		fossa.Locator{
			Fetcher:  config.Fetcher(),
			Project:  config.Project(),
			Revision: config.Revision(),
		},
		v0.UploadOptions{
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

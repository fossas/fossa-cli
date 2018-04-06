package upload

import (
	"encoding/json"
	"fmt"
	"net/url"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/api"
	"github.com/fossas/fossa-cli/cmd/fossa/common"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/services"
)

var Cmd = cli.Command{
	Name:      "upload",
	Usage:     "Uploads user-provided test results to FOSSA",
	Action:    Run,
	ArgsUsage: "<data>",
	Flags: common.WithGlobalFlags([]cli.Flag{
		cli.BoolFlag{Name: "l, locators", Usage: "upload data in locator format instead of JSON"},
	}),
}

type APIResponse struct {
	Locator string
}

func ParseLocators(locators string) api.SourceUnit {
	var deps []api.Dependency
	lines := strings.Split(locators, "\n")
	for _, line := range lines {
		deps = append(deps, api.Dependency{Locator: line})
	}
	return api.SourceUnit{
		Build: api.Build{
			Succeeded:    true,
			Dependencies: deps,
		},
	}
}

func Run(ctx *cli.Context) {
	io, c := common.MustInit(ctx)

	args := ctx.Args()
	if !args.Present() {
		io.Logger.Fatalf("No input provided.")
	}

	input := args.First()
	var data []api.SourceUnit
	if c.UploadCmd.UseLocators {
		data = []api.SourceUnit{ParseLocators(input)}
	} else {
		err := json.Unmarshal([]byte(input), &data)
		if err != nil {
			io.Logger.Fatalf("Could not parse user-provided build data: %s", err.Error())
		}
	}

	res, err := Do(io, c, data)
	if err != nil {
		io.Logger.Fatalf("Upload failed: %s", err.Error())
	}
	locatorParts := strings.Split(res.Locator, "$")
	baseURL, err := url.Parse(c.Endpoint)
	reportURL, err := url.Parse("/projects/" + url.QueryEscape(locatorParts[0]) + "/refs/branch/master/" + url.QueryEscape(locatorParts[1]) + "/browse/dependencies")
	io.Logger.Printf(`
============================================================

    View FOSSA Report:
    ` + baseURL.ResolveReference(reportURL).String() + `

============================================================
`)
}

func Do(io services.Services, c config.CLIConfig, data []api.SourceUnit) (APIResponse, error) {
	if c.Project == "" {
		io.Logger.Fatalf("Could not infer project name from either `.fossa.yml` or `git` remote named `origin`")
	}
	if c.Fetcher != "custom" && c.Revision == "" {
		io.Logger.Fatalf("Could not infer revision name from `git` remote named `origin`. To submit a custom project, set Fetcher to `custom` in `.fossa.yml`")
	}
	if len(data) == 0 {
		io.Logger.Fatalf("No data to upload")
	}

	payload, err := json.Marshal(data)
	if err != nil {
		return APIResponse{}, errors.Wrap(err, "could not marshal upload data")
	}
	io.Logger.Debugf("Uploading data from %#v modules: %#v", len(data), string(payload))

	endpoint := "/api/builds/custom?locator=" + url.QueryEscape(module.Locator{Fetcher: c.Fetcher, Project: c.Project, Revision: c.Revision}.String()) + "&v=" + c.Version
	if c.Fetcher == "custom" && c.Project == "" {
		defaultProjectTitle := data[0].Name
		cwd, err := filepath.Abs(".")
		if err != nil {
			io.Logger.Fatalf("Could not get working directory: %s", err.Error())
		}
		if cwd != "" {
			defaultProjectTitle = filepath.Base(cwd)
		}
		endpoint += fmt.Sprintf("&managedBuild=true&title=%s", url.PathEscape(defaultProjectTitle))
	}
	io.Logger.Debugf("Sending build data to %#v", endpoint)

	res, statusCode, err := io.API.Post(endpoint, payload)
	io.Logger.Debugf("Response: %#v", res)

	if statusCode == 428 {
		// TODO: handle "Managed Project" workflow
		return APIResponse{}, errors.New("invalid project or revision; make sure this version is published and FOSSA has access to your repo (to submit a custom project, set Fetcher to `custom` in `.fossa.yml`)")
	} else if statusCode == 403 {
		return APIResponse{}, errors.New("you do not have permission to upload builds for this project")
	} else if err != nil {
		return APIResponse{}, errors.Wrap(err, "could not upload")
	}
	io.Logger.Debugf("Upload finished")

	var unmarshalled APIResponse
	err = json.Unmarshal([]byte(res), &unmarshalled)
	if err != nil {
		return APIResponse{}, errors.Errorf("bad response: %s", res)
	}

	return unmarshalled, nil
}

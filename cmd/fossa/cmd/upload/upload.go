package upload

import (
	"encoding/json"
	"io/ioutil"
	"net/url"
	"os"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/cliutil"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
)

var Cmd = cli.Command{
	Name:      "upload",
	Usage:     "Uploads user-provided test results to FOSSA",
	Action:    Run,
	ArgsUsage: "<data>",
	Flags: flags.WithGlobalFlags([]cli.Flag{
		cli.BoolFlag{Name: "l, locators", Usage: "upload data in locator format (instead of JSON)"},
	}),
}

type APIResponse struct {
	Locator string
}

func ParseLocators(locators string) (fossa.SourceUnit, error) {
	var deps []fossa.Dependency
	lines := strings.Split(locators, "\n")
	for _, line := range lines {
		deps = append(deps, fossa.Dependency{Locator: line})
	}
	return fossa.SourceUnit{
		Build: fossa.Build{
			Succeeded:    true,
			Dependencies: deps,
		},
	}, nil // TODO: validate the locators
}

func hasPipeInput() bool {
	stat, err := os.Stdin.Stat()
	if err != nil {
		log.Logger.Warningf("Could not read stdin")
		return false
	}
	return (stat.Mode() & os.ModeCharDevice) == 0
}

func getInput(ctx *cli.Context, usingLocators bool) ([]fossa.SourceUnit, error) {
	// Read input
	var raw string
	if hasPipeInput() {
		stdin, err := ioutil.ReadAll(os.Stdin)
		if err != nil {
			return nil, errors.Wrap(err, "could not read stdin")
		}
		raw = string(stdin)
	} else {
		args := ctx.Args()
		if !args.Present() {
			return nil, errors.New("no input provided")
		}
		raw = args.First()
	}

	// Parse input
	if usingLocators {
		sourceUnit, err := ParseLocators(raw)
		if err != nil {
			return nil, errors.Wrap(err, "could not parse build data")
		}
		return []fossa.SourceUnit{sourceUnit}, nil
	} else {
		var out []fossa.SourceUnit
		err := json.Unmarshal([]byte(raw), &out)
		if err != nil {
			return nil, errors.Wrap(err, "could not parse build data")
		}
		return out, nil
	}
}

func Run(ctx *cli.Context) {
	c := cliutil.Init(ctx)

	data, err := getInput(ctx, c.UploadCmd.UseLocators)
	if err != nil {
		log.Logger.Fatalf("Bad input: %s", err.Error())
	}

	locator, err := Do(c, data)
	if err != nil {
		log.Logger.Fatalf("Upload failed: %s", err.Error())
	}
	baseURL, err := url.Parse(c.Endpoint)
	reportURL, err := url.Parse("/projects/" + url.QueryEscape(locator.Fetcher+"+"+locator.Project) + "/refs/branch/master/" + url.QueryEscape(locator.Revision) + "/browse/dependencies")
	log.Printf(`
============================================================

    View FOSSA Report:
    ` + strings.Replace(baseURL.ResolveReference(reportURL).String(), "%", "%%", -1) + `

============================================================
`)
}

func Do(c config.CLIConfig, data []fossa.SourceUnit) (fossa.Locator, error) {
	if c.Project == "" {
		log.Logger.Fatalf("Could not infer project name from either `.fossa.yml` or `git` remote named `origin`")
	}
	if c.Fetcher != "custom" && c.Revision == "" {
		log.Logger.Fatalf("Could not infer revision name from `git` remote named `origin`. To submit a custom project, set Fetcher to `custom` in `.fossa.yml`")
	}
	if len(data) == 0 {
		log.Logger.Fatalf("No data to upload")
	}

	title := data[0].Name
	if c.Fetcher == "custom" {
		cwd, err := filepath.Abs(".")
		if err != nil {
			log.Logger.Fatalf("Could not get working directory: %s", err.Error())
		}
		if cwd != "" {
			title = filepath.Base(cwd)
		}
	}

	return fossa.Upload(c.Fetcher, c.Project, c.Revision, title, data)
}

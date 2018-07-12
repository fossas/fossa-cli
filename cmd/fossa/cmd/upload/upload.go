package upload

import (
	"encoding/json"
	"io/ioutil"
	"os"
	"strings"

	"github.com/pkg/errors"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/cmdutil"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
)

// Command-specific flags for `fossa upload`.
var (
	Locators = "locators"
)

var Cmd = cli.Command{
	Name:      "upload",
	Usage:     "Uploads user-provided test results to FOSSA",
	Action:    Run,
	ArgsUsage: "DATA",
	Flags: flags.WithGlobalFlags([]cli.Flag{
		cli.BoolFlag{Name: flags.Short(Locators), Usage: "upload data in locator format (instead of JSON)"},
	}),
}

type APIResponse struct {
	Locator string
}

func ParseLocators(locators string) (fossa.SourceUnit, error) {
	var deps []fossa.SourceUnitDependency
	lines := strings.Split(locators, "\n")
	for _, line := range lines {
		deps = append(deps, fossa.SourceUnitDependency{Locator: line})
	}
	return fossa.SourceUnit{
		Build: fossa.SourceUnitBuild{
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
	err := cmdutil.InitWithAPI(ctx)
	if err != nil {
		log.Logger.Fatalf("Could not initialize: %s", err.Error())
	}

	data, err := getInput(ctx, ctx.Bool(Locators))
	if err != nil {
		log.Logger.Fatalf("Bad input: %s", err.Error())
	}

	log.ShowSpinner("Uploading...")
	locator, err := Do(data)
	if err != nil {
		log.Logger.Fatalf("Upload failed: %s", err.Error())
	}
	log.StopSpinner()
	log.Printf(cmdutil.FmtReportURL(locator))
}

func Do(data []fossa.SourceUnit) (fossa.Locator, error) {
	if config.Project() == "" {
		log.Logger.Fatalf("Could not infer project name from either `.fossa.yml` or `git` remote named `origin`")
	}
	if config.Fetcher() != "custom" && config.Revision() == "" {
		log.Logger.Fatalf("Could not infer revision name from `git` remote named `origin`. To submit a custom project, set Fetcher to `custom` in `.fossa.yml`")
	}
	if len(data) == 0 {
		log.Logger.Fatalf("No data to upload")
	}

	return fossa.Upload(config.Fetcher(), config.Project(), config.Revision(), config.Title(), config.Branch(), data)
}

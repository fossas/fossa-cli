// Package upload implements `fossa upload`.
//
// `fossa upload` provides manual overrides for users whose builds are too
// complex or too custom for `fossa analyze` to successfully analyze. It
// essentially acts as a thin wrapper on top of the FOSSA API.
//
// This command has multiple upload modes which accept different kinds of input.
// The upload modes are:
//
//   1. Raw mode (default): uses the raw JSON that the CLI would normally
//      upload. This is a thin wrapper over cURL.
//   2. Locator mode: provides a newline-delimited list of locators. This is the
//      simplest data format to generate, but also provides the least metadata.
//      Most simple shell scripts could probably generate this.
//   3. Graph mode (not implemented): provides the JSON dependency graph format
//      used internally by the CLI. This is a good balance between complexity
//      and ease-of-use. Generating this probably needs a proper script.
//   4. Tarball mode (not implemented): provides a folder to upload as a project
//      tarball.
package upload

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"strings"

	"github.com/pkg/errors"
	"github.com/urfave/cli"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/setup"
	"github.com/fossas/fossa-cli/config"
)

// Command-specific flags for `fossa upload`.
var (
	Locators = "locators"
)

// Cmd is the CLI command struct for uploading.
var Cmd = cli.Command{
	Name:      "upload",
	Usage:     "Uploads user-provided test results to FOSSA",
	Action:    Run,
	ArgsUsage: "DATA",
	Flags: flags.WithAPIFlags(flags.WithGlobalFlags([]cli.Flag{
		cli.BoolFlag{Name: flags.Short(Locators), Usage: "upload data in locator format (instead of JSON)"},
	})),
}

// ParseLocators parses newline-delimited string locators into SourceUnits.
func ParseLocators(locators string) (fossa.SourceUnit, error) {
	// Handle empty input.
	if locators == "" {
		return fossa.SourceUnit{}, errors.New("upload did not receive any input")
	}

	// Handle bad input with empty lines.
	var deps []fossa.SourceUnitDependency
	lines := strings.Split(locators, "\n")
	for _, line := range lines {
		if line == "" {
			continue
		}
		deps = append(deps, fossa.SourceUnitDependency{Locator: line, Imports: make([]string, 0)})
	}

	// TODO: validate the locators.

	return fossa.SourceUnit{
		Build: fossa.SourceUnitBuild{
			Succeeded:    true,
			Dependencies: deps,
		},
	}, nil
}

func hasPipeInput() bool {
	stat, err := os.Stdin.Stat()
	if err != nil {
		log.Warnf("Could not read stdin")
		return false
	}
	return (stat.Mode() & os.ModeCharDevice) == 0
}

func getInput(ctx *cli.Context, usingLocators bool) ([]fossa.SourceUnit, error) {
	// Read input.
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

	// Parse input.
	if usingLocators {
		sourceUnit, err := ParseLocators(raw)
		if err != nil {
			return nil, errors.Wrap(err, "could not parse build data")
		}
		return []fossa.SourceUnit{sourceUnit}, nil
	}

	var out []fossa.SourceUnit
	err := json.Unmarshal([]byte(raw), &out)
	if err != nil {
		return nil, errors.Wrap(err, "could not parse build data")
	}
	return out, nil
}

// Run executes the upload command.
func Run(ctx *cli.Context) {
	err := setup.SetContext(ctx)
	if err != nil {
		log.Fatalf("Could not initialize: %s", err.Error())
	}

	data, err := getInput(ctx, ctx.Bool(Locators))
	if err != nil {
		log.Fatalf("Bad input: %s", err.Error())
	}

	display.InProgress("Uploading...")
	locator, err := Do(data)
	if err != nil {
		log.Fatalf("Upload failed: %s", err.Error())
	}
	display.ClearProgress()
	fmt.Printf(locator.ReportURL())
}

// Do performs a SourceUnit upload of the current project without other side
// effects.
func Do(data []fossa.SourceUnit) (fossa.Locator, error) {
	return fossa.Upload(
		config.Title(),
		fossa.Locator{
			Fetcher:  config.Fetcher(),
			Project:  config.Project(),
			Revision: config.Revision(),
		},
		fossa.UploadOptions{
			Branch:         config.Branch(),
			ProjectURL:     config.ProjectURL(),
			JIRAProjectKey: config.JIRAProjectKey(),
		},
		data)
}

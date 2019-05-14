package test

import (
	"encoding/json"
	"fmt"
	"os"
	"time"

	"github.com/apex/log"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/api"
	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/setup"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/errors"
)

const defaultTestTemplate = `Test Failed! {{.Count}} {{if gt .Count 1 -}} issues {{- else -}} issue {{- end}} found.
{{- $length := len .NormalizedByType -}} {{ if eq $length 0 }}
Detailed issue data cannot be read. This could be related to using a push-only API token.
{{- end}}
{{- range $type, $issues := .NormalizedByType}}
========================================================================
{{$type}}
========================================================================
Dependency	Revision	{{if or (eq $type "Flagged by Policy") (eq $type "Denied by Policy") -}} License {{- end}}
{{- range $i, $issue := $issues}}
{{$issue.Name}}	{{$issue.Revision}}	{{$issue.Rule.License }}
{{- end}}
{{end}}
`

const (
	Timeout        = "timeout"
	SuppressIssues = "suppress-issues"
	JSON           = "json"
)

const pollRequestDelay = 8 * time.Second

var Cmd = cli.Command{
	Name:   "test",
	Usage:  "Test current revision against FOSSA scan status and exit with errors if issues are found",
	Action: Run,
	Flags: flags.WithGlobalFlags(flags.WithAPIFlags([]cli.Flag{
		cli.IntFlag{Name: Timeout, Value: 10 * 60, Usage: "duration to wait for build completion (in seconds)"},
		cli.BoolFlag{Name: SuppressIssues, Usage: "don't exit on stderr if issues are found"},
		cli.BoolFlag{Name: JSON, Usage: "format failed issues as JSON"},
	})),
}

var _ cli.ActionFunc = Run

func Run(ctx *cli.Context) error {
	err := setup.SetContext(ctx)
	if err != nil {
		log.Fatalf("Could not initialize: %s", err.Error())
	}

	issues, err := Do(time.After(time.Duration(ctx.Int(Timeout)) * time.Second))
	if err != nil {
		log.Fatalf("Could not test revision: %s", err.Error())
	}

	if issues.Count == 0 {
		fmt.Fprintln(os.Stderr, "Test Passed! 0 issues found")
		return nil
	}

	output := ""
	if ctx.Bool(JSON) {
		json, err := json.Marshal(issues)
		if err != nil {
			return err
		}
		output = string(json)
	} else {
		output, err = display.TemplateFormatTabs(defaultTestTemplate, issues, 12, 10, 5)
		if err != nil {
			return err
		}
	}
	fmt.Println(output)

	if !ctx.Bool(SuppressIssues) {
		os.Exit(1)
	}
	return nil
}

func Do(stop <-chan time.Time) (fossa.Issues, error) {
	defer display.ClearProgress()
	display.InProgress("Waiting for analysis to complete...")

	revision := config.Revision()
	if revision == "" {
		return fossa.Issues{}, errors.New("could not detect current revision (please set with --revision)")
	}

	project := fossa.Locator{
		Fetcher:  config.Fetcher(),
		Project:  config.Project(),
		Revision: revision,
	}

	_, err := CheckBuild(project, stop)
	if err != nil {
		log.Fatalf("Could not load build: %s", err.Error())
	}

	issues, err := CheckIssues(project, stop)
	if err != nil {
		log.Fatalf("Could not load issues: %s", err.Error())
	}

	display.ClearProgress()
	return issues, nil
}

func CheckBuild(locator fossa.Locator, stop <-chan time.Time) (fossa.Build, error) {
	for {
		select {
		case <-stop:
			return fossa.Build{}, errors.New("timed out while waiting for build")
		default:
			build, err := fossa.GetLatestBuild(locator)
			if _, ok := err.(api.TimeoutError); ok {
				time.Sleep(pollRequestDelay)
				continue
			}
			if err != nil {
				return fossa.Build{}, errors.Wrap(err, "error while loading build")
			}

			display.InProgress(fmt.Sprintf("Project status is %s. Waiting for FOSSA scan results...", build.Task.Status))

			switch build.Task.Status {
			case "SUCCEEDED":
				return build, nil
			case "FAILED":
				return build, fmt.Errorf("failed to analyze build #%d: %s (visit FOSSA or contact support@fossa.com)", build.ID, build.Error)
			case "CREATED", "ASSIGNED", "RUNNING":
				time.Sleep(pollRequestDelay)
			default:
				return fossa.Build{}, fmt.Errorf("unknown task status: %s", build.Task.Status)
			}
		}
	}
}

// CheckIssues polls the issues endpoint until an issue scan has been run on the latest project revision.
func CheckIssues(locator fossa.Locator, stop <-chan time.Time) (fossa.Issues, error) {
	for {
		select {
		case <-stop:
			return fossa.Issues{}, errors.New("timed out while waiting for scan")
		default:
			issues, err := fossa.GetIssues(locator)
			if _, ok := err.(api.TimeoutError); ok {
				time.Sleep(pollRequestDelay)
				continue
			}
			if err != nil {
				return fossa.Issues{}, errors.Wrap(err, "error while loading issues")
			}
			switch issues.Status {
			case "WAITING":
				time.Sleep(pollRequestDelay)
			case "SCANNED":
				return issues, nil
			default:
				return issues, fmt.Errorf("unknown task status: %s", issues.Status)
			}
		}
	}
}

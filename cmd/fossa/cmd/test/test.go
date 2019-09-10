package test

import (
	"encoding/json"
	"fmt"
	"os"
	"time"

	"github.com/urfave/cli"

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
	err := setup.SetContext(ctx, true)
	if err != nil {
		return errors.UnknownError(err, "")
	}

	issues, fErr := Do(time.After(time.Duration(ctx.Int(Timeout)) * time.Second))
	if fErr != nil {
		return fErr.WrapCause("Could not test revision")
	}

	if issues.Count == 0 {
		fmt.Fprintln(os.Stderr, "Test Passed! 0 issues found")
		return nil
	}

	output := ""
	if ctx.Bool(JSON) {
		json, err := json.Marshal(issues)
		if err != nil {
			return &errors.Error{
				Cause:           err,
				Type:            errors.Unknown,
				Troubleshooting: fmt.Sprintf("The following issues could not be marshalled into JSON:\n%+v", issues),
			}
		}
		output = string(json)
	} else {
		output, err = display.TemplateFormatTabs(defaultTestTemplate, issues, 12, 10, 5)
		if err != nil {
			return errors.UnknownError(err, "")
		}
	}
	fmt.Println(output)

	if !ctx.Bool(SuppressIssues) {
		os.Exit(1)
	}
	return nil
}

func Do(stop <-chan time.Time) (fossa.Issues, *errors.Error) {
	defer display.ClearProgress()
	display.InProgress("Waiting for analysis to complete...")

	revision := config.Revision()
	if revision == "" {
		return fossa.Issues{}, &errors.Error{
			Troubleshooting: "A revision could not be detected. Try setting revision using the --revision flag.",
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/user-guide.md/#fossa-test",
		}
	}

	project := fossa.Locator{
		Fetcher:  config.Fetcher(),
		Project:  config.Project(),
		Revision: revision,
	}

	_, err := CheckBuild(project, stop)
	if err != nil {
		return fossa.Issues{}, err.WrapCause("Could not load build:")
	}

	issues, err := CheckIssues(project, stop)
	if err != nil {
		return fossa.Issues{}, err.WrapCause("Could not load issues:")
	}

	display.ClearProgress()
	return issues, nil
}

// CheckBuild polls the latest_build endpoint until the most recently uploaded build has succeeded.
func CheckBuild(locator fossa.Locator, stop <-chan time.Time) (fossa.Build, *errors.Error) {
	for {
		select {
		case <-stop:
			return fossa.Build{}, &errors.Error{
				Cause:           errors.New("timed out while checking for build"),
				Type:            errors.Unknown,
				Troubleshooting: "The most likely reason for a timeout is that your project is still being analyzed on fossa.com. If your project is exceptionally large and analysis takes longer than the default timeout you can modify it with the `timeout` flag.",
				Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/user-guide.md/#fossa-test",
			}
		default:
			build, err := fossa.GetLatestBuild(locator)
			if err != nil {
				return fossa.Build{}, err
			}

			display.InProgress(fmt.Sprintf("Project status is %s. Waiting for FOSSA scan results...", build.Task.Status))

			switch build.Task.Status {
			case "SUCCEEDED":
				return build, nil
			case "FAILED":
				return build, &errors.Error{
					Cause:           fmt.Errorf("FOSSA failed to analyze build with ID #%d: %s", build.ID, build.Error),
					Type:            errors.Unknown,
					Troubleshooting: "Look at the logs for this build on app.fossa.com on the `Activity` page associated with this project to diagnose the root cause of this issue.",
				}

			case "CREATED", "ASSIGNED", "RUNNING":
				time.Sleep(pollRequestDelay)
			default:
				return fossa.Build{}, errors.UnknownError(errors.Errorf("unknown task status while waiting for FOSSA to analyze build: %s", build.Task.Status), "")
			}
		}
	}
}

// CheckIssues polls the issues endpoint until an issue scan has been run on the latest project revision.
func CheckIssues(locator fossa.Locator, stop <-chan time.Time) (fossa.Issues, *errors.Error) {
	for {
		select {
		case <-stop:
			return fossa.Issues{}, &errors.Error{
				Cause:           errors.New("timed out while waiting for issues"),
				Troubleshooting: "The most likely reason for a timeout is that your project has not had an issue scan run on fossa.com. If this is not the case and this issue persists you can increase the timeout with the `timeout` flag.",
				Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/user-guide.md/#fossa-test",
			}
		default:
			issues, err := fossa.GetIssues(locator)
			if err != nil {
				return fossa.Issues{}, err
			}
			switch issues.Status {
			case "WAITING":
				time.Sleep(pollRequestDelay)
			case "SCANNED":
				return issues, nil
			default:
				return issues, errors.UnknownError(errors.Errorf("unknown task status while loading issues: %s", issues.Status), "")
			}
		}
	}
}

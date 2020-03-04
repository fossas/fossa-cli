package analyze

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"strings"
	"time"

	"github.com/apex/log"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/setup"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

var ShowOutput = "output"
var ServerScan = "server-scan"

var Cmd = cli.Command{
	Name:      "analyze",
	Usage:     "Analyze built dependencies",
	Action:    Run,
	ArgsUsage: "MODULE",
	Flags: flags.WithGlobalFlags(flags.WithAPIFlags(flags.WithOptions([]cli.Flag{
		cli.BoolFlag{Name: "show-output, output, o", Usage: "print results to stdout instead of uploading to FOSSA"},
		cli.BoolFlag{Name: ServerScan, Usage: "run a server side dependency scan instead of a raw license scan (only raw modules)"},
		flags.TemplateF,
	}))),
}

var _ cli.ActionFunc = Run

var timeout = "timeout"
var success = "success"
var panic = "panic"

type spectrometerOutput struct {
	Status  string
	Stdout  string
	Stderr  string
	Runtime time.Duration
	Error   error
}

type comparisonData struct {
	Project      string
	Spectrometer spectrometerOutput
	CliAnalysis  []fossa.SourceUnit
	Modules      []strippedModule
	Runtime      time.Duration
	CliError     error
	Message      string
}

type strippedModule struct {
	Name        string
	Type        pkg.Type
	BuildTarget string
	Dir         string
	Options     map[string]interface{}
	Source      fossa.SourceUnit
}

// Check that the server can be hit and that it validates the run.
// The server has the ability to be a killswitch.
func authorizedComparison() (bool, int) {
	return true, 1
}

// This function runs v2 analysis and uploads the results for comparison.
// The default timeout for this function is 1 minute and the completion channel is
// used to timeout analysis if it is slower.
func v2Analysis(completion chan spectrometerOutput) {
	defer func() {
		if r := recover(); r != nil {
			completion <- spectrometerOutput{
				Status: panic,
			}
		}
	}()

	start := time.Now()
	stdout, stderr, err := exec.Run(exec.Cmd{
		Name:    "hscli",
		Argv:    []string{"scan"},
		Timeout: "1m",
		Retries: 0,
	})

	if err != nil {
		if strings.Contains(err.Error(), "operation timed out running") {
			completion <- spectrometerOutput{
				Status: timeout,
			}
			return
		}
	}

	completion <- spectrometerOutput{
		Status:  success,
		Stdout:  stdout,
		Stderr:  stderr,
		Runtime: time.Since(start),
	}
}

func uploadComparisonData(data comparisonData) {
	output, err := json.Marshal(data)
	if err != nil {
		return
	}

	u, err := url.Parse("http://localhost:3000/results")
	if err != nil {
		return
	}
	var defaultClient = http.Client{
		Timeout: 60 * time.Second,
		Transport: &http.Transport{
			DisableKeepAlives: true,
			Proxy:             http.ProxyFromEnvironment,
		},
	}

	req, err := http.NewRequest("POST", u.String(), bytes.NewReader(output))
	if err != nil {
		return
	}
	req.Header.Set("Content-Type", "application/json")
	_, err = defaultClient.Do(req)
	if err != nil {
		return
	}
}

func Run(ctx *cli.Context) error {
	startTime := time.Now()
	v2Completion := make(chan spectrometerOutput, 1)

	authorized, _ := authorizedComparison()
	if authorized && !ctx.Bool(ShowOutput) {
		go v2Analysis(v2Completion)
	}

	var err error
	normalized := []fossa.SourceUnit{}
	modules := []module.Module{}

	defer func() {
		var message string
		var spectrometerResult spectrometerOutput

		select {
		case result := <-v2Completion:
			spectrometerResult = result
		case <-time.After(5 * time.Second):
			message = "cli v1 was faster than spectrometer and forced spectrometer to exit"
		}

		if ctx.Bool(ShowOutput) {
			displaySourceunits(normalized, ctx)
		}

		comparison := comparisonData{
			Project:      config.Project(),
			Spectrometer: spectrometerResult,
			CliAnalysis:  normalized,
			Runtime:      time.Since(startTime),
			CliError:     err,
			Message:      message,
		}

		for _, module := range modules {
			sourceUnit, _ := fossa.SourceUnitFromModule(module)
			comparison.Modules = append(comparison.Modules, strippedModule{
				Name:        module.Name,
				Type:        module.Type,
				BuildTarget: module.BuildTarget,
				Dir:         module.Dir,
				Options:     module.Options,
				Source:      sourceUnit,
			})
		}

		uploadComparisonData(comparison)
	}()

	err = setup.SetContext(ctx, !ctx.Bool(ShowOutput))
	if err != nil {
		log.Fatalf("Could not initialize %s", err)
	}

	modules, err = config.Modules()
	if err != nil {
		log.Fatalf("Could not parse modules: %s", err.Error())
	}
	if len(modules) == 0 {
		log.Fatal("No modules specified.")
	}

	rawModuleLicenseScan := !ctx.Bool(ServerScan)
	analyzed, err := Do(modules, !ctx.Bool(ShowOutput), rawModuleLicenseScan)
	if err != nil {
		log.Fatalf("Could not analyze modules: %s", err.Error())
		return err
	}

	// Return if we only analyzed a raw module because we do not need to insert it into a dependency graph.
	if !rawModuleLicenseScan && len(analyzed) == 0 {
		return nil
	}

	log.Debugf("analyzed: %#v", analyzed)
	normalized, err = fossa.Normalize(analyzed)
	if err != nil {
		log.Fatalf("Could not normalize output: %s", err.Error())
		return err
	}

	// Return early and handle the print in the defer after the CLI v2 timeout.
	if ctx.Bool(ShowOutput) {
		return nil
	}

	return uploadAnalysis(normalized)
}

func displaySourceunits(sourceUnits []fossa.SourceUnit, ctx *cli.Context) {
	if tmplFile := ctx.String(flags.Template); tmplFile != "" {
		output, err := display.TemplateFile(tmplFile, sourceUnits)
		fmt.Println(output)
		if err != nil {
			log.Fatalf("Could not parse template data: %s", err.Error())
		}
	} else {
		_, err := display.JSON(sourceUnits)
		if err != nil {
			log.Fatalf("Could not serialize to JSON: %s", err.Error())
		}
	}

}

// Do runs the analysis function for all modules and also handles raw module uploads. `rawModuleLicenseScan` determines whether
// FOSSA core should only run a complete license scan or it should treat the upload as an independent project and attempt
// to find dependencies.
func Do(modules []module.Module, upload, rawModuleLicenseScan bool) (analyzed []module.Module, err error) {
	defer display.ClearProgress()
	for i, m := range modules {
		display.InProgress(fmt.Sprintf("Analyzing module (%d/%d): %s", i+1, len(modules), m.Name))

		// Handle raw modules differently from all others.
		// TODO: maybe this should occur during the analysis step?
		// TODO: maybe this should target a third-party folder, rather than a single
		// folder? Maybe "third-party folder" should be a separate module type?
		if m.Type == pkg.Raw {
			locator, err := fossa.UploadTarball(m.BuildTarget, rawModuleLicenseScan, rawModuleLicenseScan, upload)
			if err != nil {
				log.Warnf("Could not upload raw module: %s", err.Error())
			}

			// If the scan is not a raw license scan then we should not attempt to create a dependency graph.
			if !rawModuleLicenseScan {
				fmt.Printf("\nReport for raw module: %s", locator.ReportURL())
				continue
			}

			id := pkg.ID{
				Type:     pkg.Raw,
				Name:     locator.Project,
				Revision: locator.Revision,
			}
			m.Imports = []pkg.Import{pkg.Import{Resolved: id}}
			m.Deps = make(map[pkg.ID]pkg.Package)
			m.Deps[id] = pkg.Package{
				ID: id,
			}
			analyzed = append(analyzed, m)
			continue
		}

		analyzer, err := analyzers.New(m)
		if err != nil {
			analyzed = append(analyzed, m)
			log.Warnf("Could not load analyzer: %s", err.Error())
			continue
		}
		built, err := analyzer.IsBuilt()
		if err != nil {
			log.Warnf("Could not determine whether module is built: %s", err.Error())
		}
		if !built {
			log.Warnf("Module does not appear to be built")
		}
		deps, err := analyzer.Analyze()
		if err != nil {
			log.Fatalf("Could not analyze: %s", err.Error())
		}
		m.Imports = deps.Direct
		m.Deps = deps.Transitive
		analyzed = append(analyzed, m)
	}
	display.ClearProgress()

	return analyzed, err
}

func uploadAnalysis(normalized []fossa.SourceUnit) error {
	display.InProgress("Uploading analysis...")
	locator, err := fossa.Upload(
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

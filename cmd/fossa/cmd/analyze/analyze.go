package analyze

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"sort"
	"strconv"
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
var DevDependencies = "dev"

var Cmd = cli.Command{
	Name:      "analyze",
	Usage:     "Analyze built dependencies",
	Action:    Run,
	ArgsUsage: "MODULE",
	Flags: flags.WithGlobalFlags(flags.WithAPIFlags(flags.WithOptions([]cli.Flag{
		cli.BoolFlag{Name: "show-output, output, o", Usage: "print results to stdout instead of uploading to FOSSA"},
		cli.BoolFlag{Name: DevDependencies, Usage: "Include development dependencies. CAUTION: valid only for nodejs analysis."},
		cli.BoolFlag{Name: ServerScan, Usage: "run a server side dependency scan instead of a raw license scan (only raw modules)"},
		flags.TemplateF,
	}))),
}

var _ cli.ActionFunc = Run

const timeout = "timeout"
const success = "success"
const panic = "panic"
const comparisonEndpoint = "https://comparator.prod-us-west-2.cluster.fossa.io"

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

// Check that the server exists and that it validates the run.
// The server has the ability to be a killswitch and set the timeout.
func authorizedComparison() (bool, int) {
	resp, err := http.Get(comparisonEndpoint + "/authorize")
	if err != nil || resp.StatusCode != 200 {
		return false, 0
	}

	bodyBytes, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return false, 0
	}

	i, err := strconv.Atoi(string(bodyBytes))
	if err != nil {
		return false, 0
	}

	return true, i
}

func uploadComparisonData(data comparisonData) {
	output, err := json.Marshal(data)
	if err != nil {
		return
	}

	u, err := url.Parse(comparisonEndpoint + "/results")
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
	err := setup.SetContext(ctx, !ctx.Bool(ShowOutput))
	if err != nil {
		log.Fatalf("Could not initialize %s", err)
	}

	var timeout int
	authorized := false
	v2Completion := make(chan spectrometerOutput, 1)
	if !ctx.Bool(ShowOutput) && config.Endpoint() == config.DefaultEndpoint {
		authorized, timeout = authorizedComparison()
		if authorized {
			go v2Analysis(v2Completion)
		}
	}

	startTime := time.Now()
	normalized := []fossa.SourceUnit{}
	modules := []module.Module{}
	defer func() {
		if authorized {
			var message string
			var spectrometerResult spectrometerOutput

			select {
			case result := <-v2Completion:
				spectrometerResult = result
			case <-time.After(time.Duration(timeout) * time.Second):
				message = "cli v1 was faster than spectrometer and forced spectrometer to exit"
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
				sourceUnit, srcErr := fossa.SourceUnitFromModule(module)
				if srcErr != nil {
					continue
				}

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
		}

		if ctx.Bool(ShowOutput) {
			displaySourceunits(normalized, ctx)
		}
	}()

	modules, err = config.Modules()
	if err != nil {
		log.Fatalf("Could not parse modules: %s", err.Error())
	}
	if len(modules) == 0 {
		log.Fatal("No modules specified.")
	}

	rawModuleLicenseScan := !ctx.Bool(ServerScan)
	analyzed, err := Do(modules, !ctx.Bool(ShowOutput), rawModuleLicenseScan, ctx.Bool(DevDependencies))
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

	locator, err := uploadAnalysis(normalized)
	if err != nil {
		return err
	}

	// At this point, we should allow failures to be logged, but not fail the process.
	// Both of the functions below log failures and return safely.
	contributors := fetchGitContibutors()
	if contributors != nil {
		fossa.UploadContributors(contributors, locator)
	}
	return nil
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
func Do(modules []module.Module, upload, rawModuleLicenseScan, devDeps bool) (analyzed []module.Module, err error) {
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
			m.Imports = []pkg.Import{{Resolved: id}}
			m.Deps = make(map[pkg.ID]pkg.Package)
			m.Deps[id] = pkg.Package{
				ID: id,
			}
			analyzed = append(analyzed, m)
			continue
		}

		analyzer, err := analyzers.New(m, devDeps)
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

func uploadAnalysis(normalized []fossa.SourceUnit) (fossa.Locator, error) {
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
			Policy:         config.Policy(),
		},
		normalized)
	display.ClearProgress()
	if err != nil {
		log.Fatalf("Error during upload: %s", err.Error())
		return fossa.Locator{}, err
	}
	fmt.Println(locator.ReportURL())
	return locator, nil
}

func fetchGitContibutors() map[string]string {
	fmtSince := time.Now().UTC().AddDate(0, 0, -90).Format("2006-01-02")

	// the format arg produces newline-separated lines of: <author-email> :: <commit date>
	// We use commit date since some people backdate authorship
	// dates are forced into YYYY-MM-DD format.
	cmd := exec.Cmd{
		Name:    "git",
		Argv:    []string{"log", "--since", fmtSince, "--format=%ae :: %cd", "--date=short"},
		Timeout: "10s",
	}

	output, _, err := exec.Run(cmd)
	if err != nil {
		log.Debugf("Failed to run 'git log': %s", err.Error())
		return nil
	}

	contributorDates := make(map[string]string)
	scanner := bufio.NewScanner(strings.NewReader(output))
	for scanner.Scan() {
		items := strings.Split(scanner.Text(), "::")
		if len(items) != 2 {
			// We control the output format, but there may be an empty line
			continue
		}

		email := items[0]
		date := items[1]

		if oldDate := contributorDates[email]; oldDate != "" {
			// Only the newest date is relevant for current contributions
			dates := []string{date, oldDate}
			sort.Strings(dates)
			latest := dates[1]
			contributorDates[email] = latest
		} else {
			contributorDates[email] = date
		}
	}

	if err := scanner.Err(); err != nil {
		log.Debugf("Error while scanning output: %s", err)
		return nil
	}

	return contributorDates
}

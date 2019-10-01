package analyze

import (
	"context"
	"encoding/json"
	"fmt"
	"runtime"
	"sync"

	"github.com/apex/log"
	"github.com/urfave/cli"
	"golang.org/x/sync/semaphore"

	"github.com/fossas/fossa-cli/analyzers/python"
	"github.com/fossas/fossa-cli/analyzers/gradle"
	"github.com/fossas/fossa-cli/analyzers/ruby"
	"github.com/fossas/fossa-cli/analyzers/nodejs"
	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/setup"
	"github.com/fossas/fossa-cli/module"
)

const (
	JobsFlag = "jobs"
)

var NewCmd = cli.Command{
	Name:      "new-analyze",
	Usage:     "Analyze built dependencies",
	Action:    NewRun,
	ArgsUsage: "MODULE",
	Flags: flags.WithGlobalFlags(flags.WithAPIFlags(flags.WithOptions([]cli.Flag{
		cli.BoolFlag{Name: "show-output, output, o", Usage: "print results to stdout instead of uploading to FOSSA"},
		cli.IntFlag{Name: JobsFlag, Usage: "number of simultaneous jobs to run during analysis"},
		flags.TemplateF,
	}))),
	Hidden: true,
}

var Analyzers = []module.AnalyzerV2{
	nodejs.NodeAnalyzer,
	python.PythonAnalyzer,
	ruby.RubyAnalyzer,
	gradle.GradleAnalyzer,
}

// TODO: progress indicators, ...
func NewRun(ctx *cli.Context) error {
	err := setup.SetContext(ctx, !ctx.Bool(ShowOutput))
	if err != nil {
		log.Fatalf("Could not initialize %s", err)
		return err
	}

	// TODO: config
	// This will require:
	// - writing discovered strategies out to config (when a flag is set)
	//   use the DiscoverFunc on each analyzer to do this
	// - reading modules from config && bypassing discovery when a config file is present
	//   when bypassing: use AnalyzerV2.ScanModule(folder, strategies)
	//   when reading: sanity-check strategy names
	// - incorporating vcs discovery and other configuration from v1

	jobs := ctx.Int(JobsFlag)
	if jobs < 1 {
		jobs = runtime.NumCPU()
	}
	res := NewDo(jobs)

	// TODO: remove the below -- just for demo / dev purposes

	marshalled, _ := json.Marshal(fossa.ApiFormatModules(res))
	fmt.Println(string(marshalled))

	// TODO: normalize, show output (flag), upload
	return nil
}

func NewDo(jobs int) module.AnalyzerV2Output {
	disp := display.StartDisplay()
	defer disp.Stop()

	ctx := context.Background()
	sem := semaphore.NewWeighted(int64(jobs))
	var wg sync.WaitGroup

	startJob := func() error {
		return sem.Acquire(ctx, 1)
	}

	endJob := func() {
		sem.Release(1)
	}

	analysisOutput := make(chan module.AnalyzerV2Output)

	for _, analyzer := range Analyzers {
		wg.Add(1)
		go func(analyzer module.AnalyzerV2) {
			defer wg.Done()

			// TODO: pass out an error here?
			progress := disp.StartProcess(analyzer.Name)
			results := runAnalyzer(startJob, endJob, progress, ".", analyzer)
			if results != nil {
				analysisOutput <- results
			}

		}(analyzer)
	}

	go func() {
		wg.Wait()
		close(analysisOutput)
	}()

	results := make(module.AnalyzerV2Output)

	for output := range analysisOutput {
		for k, v := range output {
			results[k] = append(results[k], v...)
		}
	}

	return results
}

func runAnalyzer(startJob func() error, endJob func(), progress display.ProgressTracker, dir module.Filepath, analyzer module.AnalyzerV2) module.AnalyzerV2Output {
	// Run discovery job
	if err := startJob(); err != nil {
		// TODO: better errors
		log.Debugf("%s: Failed to acquire semaphore for discovery: %s", analyzer.Name, err.Error())
		return nil
	}

	progress.Begin("Discovery")

	modules, err := analyzer.DiscoverFunc(dir)
	endJob()

	if err != nil {
		// TODO: better errors
		log.Debugf("%s: Module discovery failed: %s", analyzer.Name, err.Error())
		return nil
	}

	// TODO: is there a case where DiscoverFunc can panic and we never release the semaphore?

	progress.Begin("Analysis")
	var wg sync.WaitGroup
	// TODO: is map thread-safe when used between threads, when there's unique key access? might be easier than this mess
	moduleOutputs := make(chan analyzedModule)

	for moduleDir, strategies := range modules {
		wg.Add(1)
		go func(moduleDir module.Filepath, strategies module.DiscoveredStrategies) {
			defer wg.Done()
			graphs, err := analyzer.ScanModule(startJob, endJob, progress, moduleDir, strategies)
			if err != nil {
				// TODO: use better errors
				log.Errorf("%s: Module scanning at %s failed: %s", analyzer.Name, moduleDir, err.Error())
				return
			}

			moduleOutputs <- analyzedModule{
				ModuleDir: moduleDir,
				Graphs:    graphs,
			}
		}(moduleDir, strategies) // TODO: break out into its own function?
	}

	go func() {
		wg.Wait()
		close(moduleOutputs)
	}()

	results := make(module.AnalyzerV2Output)

	for analyzed := range moduleOutputs {
		results[analyzed.ModuleDir] = append(results[analyzed.ModuleDir], module.Analysis{
			AnalyzerName: analyzer.Name,
			Graphs:       analyzed.Graphs,
		})
	}

	progress.End()

	return results
}

type analyzedModule struct {
	ModuleDir module.Filepath
	Graphs    []module.TaggedGraph
}

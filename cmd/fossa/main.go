package main

import (
	"encoding/json"
	"fmt"
	"os"
	"time"

	"github.com/briandowns/spinner"
	logging "github.com/op/go-logging"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/builders"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/module"
)

// main.{version,revision} are set by linker flags in Makefile and goreleaser
var version string
var commit string

var mainLogger = logging.MustGetLogger("main")

const (
	configUsage               = "path to config file (default: .fossa.{yml,yaml})"
	projectUsage              = "the FOSSA project name (default: VCS remote 'origin')"
	revisionUsage             = "the FOSSA project's revision hash (default: VCS hash HEAD)"
	endpointUsage             = "the FOSSA server endpoint (default: https://app.fossa.io)"
	buildForceUsage           = "ignore cached build artifacts"
	analyzeOutputUsage        = "print analysis results to stdout"
	analyzeAllowResolvedUsage = "allow unresolved dependencies"
	analyzeNoUploadUsage      = "do not upload results to FOSSA"
	debugUsage                = "print debug information to stderr"
)

func main() {
	app := cli.NewApp()
	app.Name = "fossa-cli"
	app.Usage = "get dependencies from your code"
	app.Version = version + " (revision " + commit + ")"

	app.Action = defaultCmd
	app.Flags = []cli.Flag{
		cli.StringFlag{Name: "c, config", Usage: configUsage},
		cli.StringFlag{Name: "p, project", Usage: projectUsage},
		cli.StringFlag{Name: "r, revision", Usage: revisionUsage},
		cli.StringFlag{Name: "e, endpoint", Usage: endpointUsage},
		cli.StringFlag{Name: "m, modules", Usage: "the modules to build and analyze"},
		cli.BoolFlag{Name: "o, output", Usage: analyzeOutputUsage},
		cli.BoolFlag{Name: "allow-unresolved", Usage: analyzeAllowResolvedUsage},
		cli.BoolFlag{Name: "no-upload", Usage: analyzeNoUploadUsage},
		cli.BoolFlag{Name: "b, build", Usage: "run a default build in module directories if they have not been pre-built"},
		cli.BoolFlag{Name: "f, force", Usage: buildForceUsage},
		cli.BoolFlag{Name: "debug", Usage: debugUsage},
	}

	app.Commands = []cli.Command{
		{
			Name:   "build",
			Usage:  "Run a default project build",
			Action: buildCmd,
			Flags: []cli.Flag{
				// TODO: specify these using c.GlobalString?
				cli.StringFlag{Name: "c, config", Usage: configUsage},
				cli.StringFlag{Name: "m, modules", Usage: "the modules to build"},
				cli.BoolFlag{Name: "f, force", Usage: buildForceUsage},
				cli.BoolFlag{Name: "debug", Usage: debugUsage},
			},
		},
		{
			Name:   "analyze",
			Usage:  "Analyze built dependencies",
			Action: analyzeCmd,
			Flags: []cli.Flag{
				cli.StringFlag{Name: "c, config", Usage: configUsage},
				cli.StringFlag{Name: "p, project", Usage: projectUsage},
				cli.StringFlag{Name: "r, revision", Usage: revisionUsage},
				cli.StringFlag{Name: "e, endpoint", Usage: endpointUsage},
				cli.StringFlag{Name: "m, modules", Usage: "the modules to analyze"},
				cli.BoolFlag{Name: "o, output", Usage: analyzeOutputUsage},
				cli.BoolFlag{Name: "allow-unresolved", Usage: analyzeAllowResolvedUsage},
				cli.BoolFlag{Name: "no-upload", Usage: analyzeNoUploadUsage},
				cli.BoolFlag{Name: "debug", Usage: debugUsage},
			},
		},
		{
			Name:   "test",
			Usage:  "Test current revision against FOSSA scan status and exit with errors if issues are found",
			Action: testCmd,
			Flags: []cli.Flag{
				cli.StringFlag{Name: "c, config", Usage: configUsage},
				cli.StringFlag{Name: "p, project", Usage: projectUsage},
				cli.StringFlag{Name: "r, revision", Usage: revisionUsage},
				cli.StringFlag{Name: "e, endpoint", Usage: endpointUsage},
				cli.IntFlag{Name: "t, timeout", Usage: "timeout for waiting for build status in seconds", Value: 60 * 10},
				cli.BoolFlag{Name: "debug", Usage: debugUsage},
			},
		},
		{
			Name:   "upload",
			Usage:  "Uploads user-provided test results to FOSSA",
			Action: uploadCmd,
			Flags: []cli.Flag{
				cli.StringFlag{Name: "c, config", Usage: configUsage},
				cli.StringFlag{Name: "p, project", Usage: projectUsage},
				cli.StringFlag{Name: "r, revision", Usage: revisionUsage},
				cli.StringFlag{Name: "e, endpoint", Usage: endpointUsage},
				cli.StringFlag{Name: "d, data", Usage: "the user-provided build data to upload"},
				cli.BoolFlag{Name: "debug", Usage: debugUsage},
			},
		},
	}

	app.Run(os.Args)
}

func resolveModuleConfig(moduleConfig config.ModuleConfig) (builders.Builder, module.Module, error) {
	mainLogger.Debugf("Resolving ModuleConfig: %#v", moduleConfig)

	var builder builders.Builder
	var m module.Module
	var err error

	moduleType := config.GetModuleType(moduleConfig.Type)
	if moduleType == "" {
		mainLogger.Debug("Got unknown module.")
		return builder, m, fmt.Errorf("unknown module type: %s", moduleConfig.Type)
	}

	mainLogger.Debugf("Got <%s> module.", moduleType)
	builder = builders.New(moduleType)

	if builder == nil {
		mainLogger.Debug("Got unknown builder.")
		return nil, m, fmt.Errorf("no builder available for type: %s", moduleConfig.Type)
	}

	m, err = module.New(moduleType, moduleConfig)
	if err != nil {
		mainLogger.Debug("Unable to resolve module config")
		return builder, m, fmt.Errorf("unable to setup module type: %s", moduleConfig.Type)
	}

	mainLogger.Debugf("Resolved ModuleConfig to: %#v, %#v", builder, m)
	return builder, m, nil
}

func defaultCmd(c *cli.Context) {
	conf, err := config.Initialize(c)
	if err != nil {
		mainLogger.Fatalf("Could not load configuration: %s", err.Error())
	}

	if len(conf.Modules) == 0 {
		mainLogger.Fatal("No modules specified for analysis.")
	}

	s := spinner.New(spinner.CharSets[11], 100*time.Millisecond)
	s.Writer = os.Stderr
	s.Suffix = " Initializing..."
	s.Start()

	dependencies := make(analysis)

	for i, m := range conf.Modules {
		s.Suffix = fmt.Sprintf(" Running build analysis (%d/%d): %s", i+1, len(conf.Modules), m.Name)
		s.Restart()

		builder, module, err := resolveModuleConfig(m)
		if err != nil {
			s.Stop()
			buildLogger.Fatalf("Could not parse module configuration: %s", err.Error())
		}

		err = builder.Initialize()
		if err != nil {
			s.Stop()
			buildLogger.Fatalf("Failed to initialize build: %s", err.Error())
		}

		isBuilt, err := builder.IsBuilt(module, conf.AnalyzeCmd.AllowUnresolved)
		if err != nil {
			s.Stop()
			mainLogger.Fatalf("Could not determine whether module %s is built: %s", module.Name, err.Error())
		}

		if !isBuilt {
			if conf.DefaultCmd.Build {
				s.Suffix = fmt.Sprintf(" Running module build (%d/%d): %s", i+1, len(conf.Modules), m.Path)
				s.Restart()

				err := builder.Build(module, conf.BuildCmd.Force)
				if err != nil {
					s.Stop()
					mainLogger.Fatalf("Build failed (%s): %s", m.Path, err.Error())
				}
			} else {
				s.Stop()
				mainLogger.Fatalf("Module %s does not appear to be built. Try first running your build or `fossa build`, and then running `fossa`.", module.Name)
			}
		}

		s.Stop()
		s.Suffix = fmt.Sprintf(" Running module analysis (%d/%d): %s", i+1, len(conf.Modules), m.Path)
		s.Restart()
		deps, err := builder.Analyze(module, conf.AnalyzeCmd.AllowUnresolved)
		mainLogger.Debugf("Analysis complete: %#v", deps)
		s.Stop()

		dependencies[analysisKey{
			builder: builder,
			module:  module,
		}] = deps
	}

	if conf.AnalyzeCmd.Output {
		normalModules, err := normalizeAnalysis(dependencies)
		if err != nil {
			mainLogger.Fatalf("Could not normalize build data: %s", err.Error())
		}
		buildData, err := json.Marshal(normalModules)
		if err != nil {
			mainLogger.Fatalf("Could marshal analysis results: %s", err.Error())
		}
		fmt.Println(string(buildData))
	}

	if conf.AnalyzeCmd.NoUpload {
		return
	}

	s.Stop()
	s.Suffix = fmt.Sprintf(" Uploading build results (%d/%d)...", len(conf.Modules), len(conf.Modules))
	s.Restart()

	normalModules, err := normalizeAnalysis(dependencies)
	if err != nil {
		mainLogger.Fatalf("Could not normalize build data: %s", err.Error())
	}
	msg, err := doUpload(conf, normalModules)
	s.Stop()
	if err != nil {
		mainLogger.Fatalf("Upload failed: %s", err.Error())
	}
	fmt.Print(msg)
}

package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/briandowns/spinner"
	logging "github.com/op/go-logging"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/build"
	"github.com/fossas/fossa-cli/module"
)

// main.{version,revision} are set by linker flags in Makefile and goreleaser
var version string
var commit string

var mainLogger = logging.MustGetLogger("main")

const (
	configUsage               = "path to config file; defaults to .fossa.yml or .fossa.yaml"
	projectUsage              = "the FOSSA project name; defaults to VCS remote 'origin'"
	revisionUsage             = "the FOSSA project's revision hash; defaults VCS hash at HEAD"
	endpointUsage             = "the FOSSA server endpoint; defaults to https://app.fossa.io"
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
	}

	app.Run(os.Args)
}

type cliConfig struct {
	apiKey   string
	project  string
	revision string
	endpoint string
	modules  []moduleConfig
	debug    bool

	defaultConfig defaultConfig
	analyzeConfig analyzeConfig
	buildConfig   buildConfig
}

type defaultConfig struct {
	build bool
}

func initialize(c *cli.Context) (cliConfig, error) {
	var config = cliConfig{
		apiKey:   c.String("api_key"),
		project:  c.String("project"),
		revision: c.String("revision"),
		endpoint: c.String("endpoint"),
		modules:  parseModuleFlag(c.String("modules")),
		debug:    c.Bool("debug"),

		defaultConfig: defaultConfig{
			build: c.Bool("build"),
		},

		analyzeConfig: analyzeConfig{
			output:          c.Bool("output"),
			allowUnresolved: c.Bool("allow-unresolved"),
			noUpload:        c.Bool("no-upload"),
		},

		buildConfig: buildConfig{
			force: c.Bool("force"),
		},
	}

	// Load configuration file and set overrides.
	configFile, err := readConfigFile(c.String("config"))
	if err != nil {
		return cliConfig{}, err
	}

	var locatorSections []string
	var locatorProject string
	var locatorRevision string

	if configFile.CLI.Locator != "" {
		locatorSections = strings.Split(configFile.CLI.Locator, "$")
		locatorProject = locatorSections[0]
		locatorRevision = locatorSections[1]
	}

	if config.apiKey == "" {
		config.apiKey = configFile.CLI.APIKey
	}
	if config.endpoint == "" {
		config.endpoint = configFile.CLI.Server
	}
	if config.project == "" {
		config.project = configFile.CLI.Project
		if config.project == "" {
			config.project = locatorProject
		}
	}
	if config.revision == "" {
		config.revision = locatorRevision
	}
	if len(config.modules) == 0 {
		config.modules = configFile.Analyze.Modules
	}

	// Configure logging.
	formatter := logging.MustStringFormatter(
		`%{color}%{time} %{level} %{module}:%{shortpkg}/%{shortfile}/%{shortfunc}%{color:reset} %{message}`,
	)
	stderrBackend := logging.AddModuleLevel(logging.NewBackendFormatter(logging.NewLogBackend(os.Stderr, "", 0), formatter))
	stderrBackend.SetLevel(logging.WARNING, "")
	if config.debug {
		stderrBackend.SetLevel(logging.DEBUG, "")
	}
	logging.SetBackend(stderrBackend)

	mainLogger.Debugf("Configuration initialized: %+v\n", config)

	return config, nil
}

func parseModuleFlag(moduleFlag string) []moduleConfig {
	if moduleFlag == "" {
		return []moduleConfig{}
	}
	var config []moduleConfig

	modules := strings.Split(moduleFlag, ",")
	for _, m := range modules {
		sections := strings.Split(m, ":")
		config = append(config, moduleConfig{
			Name: sections[1],
			Path: sections[1],
			Type: module.Type(sections[0]),
		})
	}

	return config
}

func resolveModuleConfig(moduleConfig moduleConfig) (module.Builder, module.Module, error) {
	mainLogger.Debugf("Resolving moduleConfig: %+v\n", moduleConfig)

	var builder module.Builder
	var m module.Module

	switch moduleConfig.Type {
	case "nodejs":
		fallthrough
	case "commonjspackage":
		mainLogger.Debug("Got NodeJS module.")
		builder = &build.NodeJSBuilder{}

		modulePath, err := filepath.Abs(moduleConfig.Path)
		if filepath.Base(modulePath) == "package.json" {
			modulePath = filepath.Dir(modulePath)
		}
		if err != nil {
			return nil, m, err
		}
		moduleName := moduleConfig.Name
		if moduleName == "" {
			moduleName = moduleConfig.Path
		}
		m = module.Module{
			Name:   moduleName,
			Type:   module.Type("nodejs"),
			Target: filepath.Join(modulePath, "package.json"),
			Dir:    modulePath,
		}
	case "bower":
		mainLogger.Debug("Got Bower module.")
		builder = &build.BowerBuilder{}

		modulePath, err := filepath.Abs(moduleConfig.Path)
		if filepath.Base(modulePath) == "bower.json" {
			modulePath = filepath.Dir(modulePath)
		}
		if err != nil {
			return nil, m, err
		}
		moduleName := moduleConfig.Name
		if moduleName == "" {
			moduleName = moduleConfig.Path
		}
		m = module.Module{
			Name:   moduleName,
			Type:   module.Type("bower"),
			Target: filepath.Join(modulePath, "bower.json"),
			Dir:    modulePath,
		}
	case "composer":
		mainLogger.Debug("Got Composer module.")
		builder = &build.ComposerBuilder{}

		modulePath, err := filepath.Abs(moduleConfig.Path)
		if filepath.Base(modulePath) == "composer.json" {
			modulePath = filepath.Dir(modulePath)
		}
		if err != nil {
			return nil, m, err
		}
		moduleName := moduleConfig.Name
		if moduleName == "" {
			moduleName = moduleConfig.Path
		}
		m = module.Module{
			Name:   moduleName,
			Type:   module.Type("composer"),
			Target: filepath.Join(modulePath, "composer.json"),
			Dir:    modulePath,
		}
	default:
		mainLogger.Debug("Got unknown module.")
		return builder, m, errors.New("unknown module type: " + string(moduleConfig.Type))
	}

	mainLogger.Debugf("Resolved moduleConfig to: %+v, %+v\n", builder, m)
	return builder, m, nil
}

func defaultCmd(c *cli.Context) {
	config, err := initialize(c)
	if err != nil {
		mainLogger.Fatalf("Could not load configuration: %s\n", err.Error())
	}

	if len(config.modules) == 0 {
		mainLogger.Fatal("No modules specified for analysis.\n")
	}

	s := spinner.New(spinner.CharSets[11], 100*time.Millisecond)
	s.Writer = os.Stderr
	s.Suffix = " Initializing..."
	s.Start()

	dependencies := make(analysis)

	for i, m := range config.modules {
		s.Suffix = fmt.Sprintf(" Running build analysis (%d/%d): %s", i+1, len(config.modules), m.Name)
		s.Restart()

		builder, module, err := resolveModuleConfig(m)
		if err != nil {
			buildLogger.Fatalf("Could not parse module configuration: %s\n", err.Error())
		}

		err = builder.Initialize()
		if err != nil {
			buildLogger.Fatalf("Failed to initialize build: %s\n", err.Error())
		}

		isBuilt, err := builder.IsBuilt(module)
		if err != nil {
			mainLogger.Fatalf("Could not determine whether module %s is built.\n", module.Name)
		}

		if !isBuilt {
			if config.defaultConfig.build {
				s.Suffix = fmt.Sprintf(" Running module build (%d/%d): %s", i+1, len(config.modules), m.Path)
				s.Restart()

				err := builder.Build(module, config.buildConfig.force)
				if err != nil {
					s.Stop()
					mainLogger.Fatalf("Build failed (%s): %s\n", m.Path, err.Error())
				}
			} else {
				mainLogger.Fatalf("Module %s does not appear to be built. Try first running your build or `fossa build`, and then running `fossa`.", module.Name)
			}
		}

		s.Suffix = fmt.Sprintf(" Running module analysis (%d/%d): %s", i+1, len(config.modules), m.Path)
		s.Restart()
		deps, err := builder.Analyze(module)

		dependencies[analysisKey{
			builder: builder,
			module:  module,
		}] = deps
	}

	if config.analyzeConfig.output {
		normalModules, err := normalizeAnalysis(dependencies)
		if err != nil {
			mainLogger.Fatalf("Could not normalize build data: %s\n", err.Error())
		}
		buildData, err := json.Marshal(normalModules)
		if err != nil {
			mainLogger.Fatalf("Could marshal analysis results: %s\n", err.Error())
		}
		fmt.Println(string(buildData))
	}

	if config.analyzeConfig.noUpload {
		return
	}

	s.Suffix = fmt.Sprintf(" Uploading build results (%d/%d)...", len(config.modules), len(config.modules))
	s.Restart()
	err = doUpload(config, dependencies)
	if err != nil {
		s.Stop()
		mainLogger.Fatalf("Upload failed: %s\n", err.Error())
	}
}

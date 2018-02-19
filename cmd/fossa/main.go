package main

import (
	"encoding/json"
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
	}

	app.Run(os.Args)
}

func setupModule(config moduleConfig, manifestName string, moduleType module.Type) (module.Module, error) {
	var m module.Module

	modulePath, err := filepath.Abs(config.Path)
	if filepath.Base(modulePath) == manifestName {
		modulePath = filepath.Dir(modulePath)
	}
	if err != nil {
		return m, err
	}

	moduleName := config.Name
	if moduleName == "" {
		moduleName = config.Path
	}

	m = module.Module{
		Name:   moduleName,
		Type:   moduleType,
		Target: filepath.Join(modulePath, manifestName),
		Dir:    modulePath,
	}

	mainLogger.Debugf("Module setup complete: %#v", m)

	return m, nil
}

func resolveModuleConfig(moduleConfig moduleConfig) (module.Builder, module.Module, error) {
	mainLogger.Debugf("Resolving moduleConfig: %#v", moduleConfig)

	// Don't use `:=` within each switch case, or you'll create a new binding that
	// shadows these bindings (apparently switches create a new scope...).
	var builder module.Builder
	var m module.Module
	var err error

	switch moduleConfig.Type {
	case "commonjspackage": // Alias for backwards compatibility
		fallthrough
	case "nodejs":
		mainLogger.Debug("Got NodeJS module.")
		builder = &build.NodeJSBuilder{}
		m, err = setupModule(moduleConfig, "package.json", module.Nodejs)
		if err != nil {
			return nil, m, err
		}
	case "bower":
		mainLogger.Debug("Got Bower module.")
		builder = &build.BowerBuilder{}
		m, err = setupModule(moduleConfig, "bower.json", module.Bower)
		if err != nil {
			return nil, m, err
		}
	case "composer":
		mainLogger.Debug("Got Composer module.")
		builder = &build.ComposerBuilder{}
		m, err = setupModule(moduleConfig, "composer.json", module.Composer)
		if err != nil {
			return nil, m, err
		}
	case "gopackage":
		fallthrough
	case "golang":
		fallthrough
	case "go":
		mainLogger.Debug("Got Go module.")
		builder = &build.GoBuilder{}
		m, err = setupModule(moduleConfig, "", module.Golang)
		// Target should be relative to $GOPATH
		m.Target = strings.TrimPrefix(m.Target, filepath.Join(os.Getenv("GOPATH"), "src")+"/")
		if err != nil {
			return nil, m, err
		}
	case "maven":
		fallthrough
	case "mvn":
		mainLogger.Debug("Got Maven module.")
		builder = &build.MavenBuilder{}
		m, err = setupModule(moduleConfig, "pom.xml", module.Maven)
		if err != nil {
			return nil, m, err
		}
	case "bundler":
		fallthrough
	case "gem":
		fallthrough
	case "rubygems":
		fallthrough
	case "ruby":
		mainLogger.Debug("Got Ruby module.")
		builder = &build.RubyBuilder{}
		m, err = setupModule(moduleConfig, "Gemfile", module.Ruby)
		if err != nil {
			return nil, m, err
		}
	case "scala":
		fallthrough
	case "sbtpackage":
		fallthrough
	case "sbt":
		mainLogger.Debug("Got SBT module.")
		builder = &build.SBTBuilder{}
		m, err = setupModule(moduleConfig, "build.sbt", module.SBT)
		if err != nil {
			return nil, m, err
		}
	case "vendoredarchives":
		mainLogger.Debug("Got vendored archives module.")
		builder = &build.VendoredArchiveBuilder{}
		m, err = setupModule(moduleConfig, "", module.VendoredArchives)
		if err != nil {
			return nil, m, err
		}
	default:
		mainLogger.Debug("Got unknown module.")
		return builder, m, fmt.Errorf("unknown module type: %s", moduleConfig.Type)
	}

	mainLogger.Debugf("Resolved moduleConfig to: %#v, %#v", builder, m)
	return builder, m, nil
}

func defaultCmd(c *cli.Context) {
	config, err := initialize(c)
	if err != nil {
		mainLogger.Fatalf("Could not load configuration: %s", err.Error())
	}

	if len(config.modules) == 0 {
		mainLogger.Fatal("No modules specified for analysis.")
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
			s.Stop()
			buildLogger.Fatalf("Could not parse module configuration: %s", err.Error())
		}

		err = builder.Initialize()
		if err != nil {
			s.Stop()
			buildLogger.Fatalf("Failed to initialize build: %s", err.Error())
		}

		isBuilt, err := builder.IsBuilt(module, config.analyzeConfig.allowUnresolved)
		if err != nil {
			s.Stop()
			mainLogger.Fatalf("Could not determine whether module %s is built: %s", module.Name, err.Error())
		}

		if !isBuilt {
			if config.defaultConfig.build {
				s.Suffix = fmt.Sprintf(" Running module build (%d/%d): %s", i+1, len(config.modules), m.Path)
				s.Restart()

				err := builder.Build(module, config.buildConfig.force)
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
		s.Suffix = fmt.Sprintf(" Running module analysis (%d/%d): %s", i+1, len(config.modules), m.Path)
		s.Restart()
		deps, err := builder.Analyze(module, config.analyzeConfig.allowUnresolved)
		mainLogger.Debugf("Analysis complete: %#v", deps)
		s.Stop()

		dependencies[analysisKey{
			builder: builder,
			module:  module,
		}] = deps
	}

	if config.analyzeConfig.output {
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

	if config.analyzeConfig.noUpload {
		return
	}

	s.Stop()
	s.Suffix = fmt.Sprintf(" Uploading build results (%d/%d)...", len(config.modules), len(config.modules))
	s.Restart()
	msg, err := doUpload(config, dependencies)
	s.Stop()
	if err != nil {
		mainLogger.Fatalf("Upload failed: %s", err.Error())
	}
	fmt.Print(msg)
}

package main

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/module"
	logging "github.com/op/go-logging"
	"github.com/urfave/cli"
)

var analysisLogger = logging.MustGetLogger("analyze")

func analyzeCmd(c *cli.Context) {
	conf, err := config.New(c)
	if err != nil {
		buildLogger.Fatalf("Could not load configuration: %s", err.Error())
	}
	if len(conf.Modules) == 0 {
		buildLogger.Fatal("No modules specified.")
	}

	analysis, err := doAnalyze(conf.Modules, conf.AnalyzeCmd.AllowUnresolved)
	if err != nil {
		analysisLogger.Fatalf("Analysis failed: %s", err.Error())
	}
	analysisLogger.Debugf("Analysis complete: %#v", analysis)

	normalModules, err := normalizeAnalysis(analysis)
	if err != nil {
		analysisLogger.Fatalf("Could not normalize build data: %s", err.Error())
	}

	if conf.AnalyzeCmd.Output {
		buildData, err := json.Marshal(normalModules)
		if err != nil {
			analysisLogger.Fatalf("Could not marshal analysis results: %s", err.Error())
		}
		fmt.Println(string(buildData))
		os.Exit(0)
		return
	}

	msg, err := doUpload(conf, normalModules)
	if err != nil {
		analysisLogger.Fatalf("Upload failed: %s", err.Error())
	}
	fmt.Print(msg)
}

type analysisKey struct {
	builder module.Builder
	module  module.Module
}

type analysis map[analysisKey][]module.Dependency

func doAnalyze(modules []module.Config, allowUnresolved bool) (analysis, error) {
	analysisLogger.Debugf("Running analysis on modules: %#v", modules)
	dependencies := make(analysis)

	for _, moduleConfig := range modules {
		builder, m, err := resolveModuleConfig(moduleConfig)
		if err != nil {
			return nil, fmt.Errorf("failed to resolve modules: %s", err.Error())
		}

		err = builder.Initialize()
		if err != nil {
			return nil, fmt.Errorf("failed to initialize build: %s", err.Error())
		}

		isBuilt, err := builder.IsBuilt(m, allowUnresolved)
		if err != nil {
			return nil, fmt.Errorf("could not determine whether module %#v is built: %#v", m.Name, err.Error())
		}
		if !isBuilt {
			return nil, fmt.Errorf("module %s does not appear to be built (try first running your build or `fossa build`, and then running `fossa`)", m.Name)
		}

		deps, err := builder.Analyze(m, allowUnresolved)
		if err != nil {
			return nil, fmt.Errorf("analysis failed on module %s: %s", m.Name, err.Error())
		}
		dependencies[analysisKey{
			builder: builder,
			module:  m,
		}] = deps
	}

	return dependencies, nil
}

package main

import (
	"encoding/json"
	"errors"
	"fmt"

	"github.com/fossas/fossa-cli/module"
	logging "github.com/op/go-logging"
	"github.com/urfave/cli"
)

var analysisLogger = logging.MustGetLogger("analyze")

type analyzeConfig struct {
	output          bool
	allowUnresolved bool
	noUpload        bool
}

func analyzeCmd(c *cli.Context) {
	config, err := initialize(c)
	if err != nil {
		buildLogger.Fatalf("Could not load configuration: %s\n", err.Error())
	}
	if len(config.modules) == 0 {
		buildLogger.Fatal("No modules specified.\n")
	}

	analysis, err := doAnalyze(config.modules)
	if err != nil {
		analysisLogger.Fatalf("Analysis failed: %s\n", err.Error())
	}
	analysisLogger.Debugf("Analysis complete: %+v\n", analysis)

	if config.analyzeConfig.noUpload {
		if config.analyzeConfig.output {
			fmt.Println(json.Marshal(analysis))
		} else {
			fmt.Println("Analysis succeeded!")
		}
	} else {
		err = doUpload(config, analysis)
		if err != nil {
			analysisLogger.Fatalf("Upload failed: %s\n", err.Error())
		}
	}
}

type analysisKey struct {
	builder module.Builder
	module  module.Module
}

type analysis map[analysisKey][]module.Dependency

func doAnalyze(modules []moduleConfig) (analysis, error) {
	analysisLogger.Debugf("Running analysis on modules: %+v\n", modules)
	dependencies := make(analysis)

	for _, moduleConfig := range modules {
		builder, m, err := resolveModuleConfig(moduleConfig)
		if err != nil {
			return nil, errors.New("failed to resolve modules: " + err.Error())
		}

		err = builder.Initialize()
		if err != nil {
			return nil, errors.New("failed to initialize build: " + err.Error())
		}

		isBuilt, err := builder.IsBuilt(m)
		if err != nil {
			return nil, errors.New("could not determine whether module " + m.Name + " is built")
		}
		if !isBuilt {
			return nil, errors.New("module " + m.Name + "does not appear to be built (try first running your build or `fossa build`, and then running `fossa`)")
		}

		deps, err := builder.Analyze(m)
		if err != nil {
			return nil, errors.New("analysis failed on module " + m.Name + ": " + err.Error())
		}
		dependencies[analysisKey{
			builder: builder,
			module:  m,
		}] = deps
	}

	return dependencies, nil
}

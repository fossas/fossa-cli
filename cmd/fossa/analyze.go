package main

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/urfave/cli"
)

func analyzeCmd(c *cli.Context) {
	conf, err := config.New(c)
	if err != nil {
		log.Fatalf("Could not load configuration: %s", err.Error())
	}
	if len(conf.Modules) == 0 {
		log.Fatal("No modules specified.")
	}

	analyses, err := doAnalyze(conf.Modules, conf.AnalyzeCmd.AllowUnresolved)
	if err != nil {
		log.Fatalf("Analysis failed: %s", err.Error())
	}

	log.Debugf("Analysis complete: %#v", analysis)

	normalModules, err := normalizeAnalysis(analyses)
	if err != nil {
		log.Fatalf("Could not normalize build data: %s", err.Error())
	}

	if conf.AnalyzeCmd.Output {
		buildData, err := json.Marshal(normalModules)
		if err != nil {
			log.Fatalf("Could not marshal analysis results: %s", err.Error())
		}
		fmt.Println(string(buildData))
		os.Exit(0)
		return
	}

	msg, err := doUpload(conf, normalModules)
	if err != nil {
		log.Fatalf("Upload failed: %s", err.Error())
	}
	fmt.Print(msg)
}

type analysis struct {
	builder      module.Builder
	module       module.Module
	dependencies []module.Dependency
}

func doAnalyze(modules []module.Config, allowUnresolved bool) ([]analysis, error) {
	log.Debugf("Running analysis on modules: %#v", modules)
	analyses := []analysis{}

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
		analyses = append(analyses, analysis{
			builder:      builder,
			module:       m,
			dependencies: deps,
		})
	}

	return analyses, nil
}

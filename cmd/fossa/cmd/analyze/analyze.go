package analyze

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/services"
)

func Run(c *cli.Context) {
	conf := config.MustNew(c)
	io := services.New(false, false)

	if len(conf.Modules) == 0 {
		io.Logger.Fatalf("No modules specified.")
	}

	analysis, err := doAnalyze(conf.Modules, conf.AnalyzeCmd.AllowUnresolved)
	if err != nil {
		io.Logger.Fatalf("Analysis failed: %s", err.Error())
	}
	io.Logger.Debugf("Analysis complete: %#v", analysis)

	normalModules, err := normalizeAnalysis(analysis)
	if err != nil {
		io.Logger.Fatalf("Could not normalize build data: %s", err.Error())
	}

	if conf.AnalyzeCmd.Output {
		buildData, err := json.Marshal(normalModules)
		if err != nil {
			io.Logger.Fatalf("Could not marshal analysis results: %s", err.Error())
		}
		fmt.Println(string(buildData))
		os.Exit(0)
		return
	}

	msg, err := doUpload(conf, normalModules)
	if err != nil {
		io.Logger.Fatalf("Upload failed: %s", err.Error())
	}
	fmt.Print(msg)
}

type analysisKey struct {
	builder module.Builder
	module  module.Module
}

type analysis map[analysisKey][]module.Dependency

func Do(modules []module.Config, allowUnresolved bool) (analysis, error) {
	io.Logger.Debugf("Running analysis on modules: %#v", modules)
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

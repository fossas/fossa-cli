package main

import (
	"encoding/json"
	"fmt"
	"io"
	"text/template"

	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/urfave/cli"
)

func analyzeCmd(c *cli.Context) {
	conf, err := config.New(c)
	if err != nil {
		log.Logger.Fatalf("Could not load configuration: %s", err.Error())
	}
	if len(conf.Modules) == 0 {
		log.Logger.Fatal("No modules specified.")
	}

	analyses, err := doAnalyze(conf.Modules, conf.AnalyzeCmd.AllowUnresolved)
	if err != nil {
		log.Logger.Fatalf("Analysis failed: %s", err.Error())
	}

	log.Logger.Debugf("Analysis complete: %#v", analyses)

	normalModules, err := normalizeAnalysis(analyses)
	if err != nil {
		log.Logger.Fatalf("Could not normalize build data: %s", err.Error())
	}
	ouputAnalyze(conf, normalModules)
}

func ouputAnalyze(conf config.CLIConfig, normalModules []normalizedModule) {
	var (
		msg    []byte
		tmpl   *template.Template
		out    io.WriteCloser
		err    error
		upload = true
	)
	out, err = openOutFile("-", 0774)
	if err != nil {
		log.Logger.Fatalf("Could not open output file: %s", err.Error())
	}

	if conf.AnalyzeCmd.Template != "" {
		upload = false
		tmpl, err = template.ParseFiles(conf.AnalyzeCmd.Template)
		if err != nil {
			log.Logger.Fatalf("Could not parse template data: %s", err.Error())
		}
		msg, err = processTmpl(tmpl, normalModules)
		if err != nil {
			log.Logger.Fatalf("Could not process template data: %s", err.Error())
		}
	}

	if conf.AnalyzeCmd.Output != "" {
		upload = false
		out, err = openOutFile(conf.AnalyzeCmd.Output, 0774)
		if err != nil {
			log.Logger.Fatalf("Could not open output file: %s", err.Error())
		}
	}

	if upload {
		msg, err = doUpload(conf, normalModules)
		if err != nil {
			log.Logger.Fatalf("Upload failed: %s", err.Error())
		}
	} else if len(msg) == 0 {
		msg, err = json.Marshal(normalModules)
		if err != nil {
			log.Logger.Fatalf("Could not marshal analysis results: %s", err.Error())
		}
	}

	defer out.Close()
	_, err = out.Write(msg)
	if err != nil {
		log.Logger.Fatalf("Could write output: %s", err.Error())
	}
}

type analysis struct {
	builder      module.Builder
	module       module.Module
	dependencies []module.Dependency
}

func doAnalyze(modules []module.Config, allowUnresolved bool) ([]analysis, error) {
	log.Logger.Debugf("Running analysis on modules: %#v", modules)
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

package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/fossas/fossa-cli/build"
	"github.com/fossas/fossa-cli/log"
	logging "github.com/op/go-logging"
	"github.com/urfave/cli"
)

// main.version is picked up by goreleaser
var version = "0.0.0"

func main() {
	app := cli.NewApp()
	app.Name = "fossa-cli"
	app.Usage = "get dependencies from your code"
	app.Version = version
	app.Action = MakeCmd
	app.Flags = []cli.Flag{
		cli.StringFlag{Name: "loglevel, l"},
	}

	app.Commands = []cli.Command{
		{
			Name:    "build",
			Aliases: []string{},
			Usage:   "discover dependencies for an inline module",
			Action:  BuildCmd,
			Flags: []cli.Flag{
				cli.StringFlag{Name: "type, t"},
				cli.StringFlag{Name: "entry-point, e"},
				cli.BoolFlag{Name: "install, i"},
				cli.BoolFlag{Name: "no-cache"},
			},
		},
	}

	app.Before = BootstrapCmd

	app.Run(os.Args)
}

// BootstrapCmd initializes and loads config for the CLI
func BootstrapCmd(c *cli.Context) error {
	devNullBackend := logging.NewLogBackend(ioutil.Discard, "", 0)

	// log errors to stderr
	stderrBackend := logging.AddModuleLevel(logging.NewBackendFormatter(logging.NewLogBackend(os.Stderr, "", 0), log.Format))
	stderrBackend.SetLevel(logging.ERROR, "")

	if c.String("loglevel") == "debug" {
		stderrBackend.SetLevel(logging.DEBUG, "")
	}

	logging.SetBackend(devNullBackend, stderrBackend)

	return nil
}

// MakeCmd runs the scan and build commands
func MakeCmd(c *cli.Context) error {
	// run scan and set context
	// run build and set context
	return nil
}

// BuildCmd takes in a Module and builds it / populates dependency data
// A successful build will set Module.Resolved to true
// An unsuccessful build will set Module.Error to a value
func BuildCmd(c *cli.Context) error {
	// Read configuration file
	config, err := ReadConfig()
	if err != nil {
		return err
	}

	// Set build options
	buildType := c.String("type")
	if len(buildType) == 0 && config != nil && len(config.Analyze) > 0 {
		buildType = config.Analyze[0].Type
	}

	entryPoint := c.String("entry-point")
	if len(entryPoint) == 0 && config != nil && len(config.Analyze) > 0 {
		entryPoint = config.Analyze[0].Path
	}

	module := build.Module{
		Type: buildType,
	}

	buildOpts := make(map[string]interface{})
	buildOpts["install"] = c.Bool("install")
	buildOpts["no-cache"] = c.Bool("no-cache")
	buildOpts["entry-point"] = entryPoint

	if len(buildOpts["entry-point"].(string)) > 0 {
		// override module manifest
		module.Manifest = buildOpts["entry-point"].(string)
	}

	if err := module.Analyze(buildOpts); err != nil {
		log.Logger.Fatalf("analysis failed (%v);\ntry pre-building and then running `fossa`", err)
	}

	log.Logger.Debugf("found (%s) deduped dependencies", len(module.Build.RawDependencies))

	dat, _ := json.Marshal(module)
	fmt.Print(string(dat))
	return nil
}

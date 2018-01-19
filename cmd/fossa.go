package main

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/fossas/fossa-cli/build"
	"github.com/fossas/fossa-cli/log"
	"github.com/urfave/cli"
)

func main() {
	app := cli.NewApp()
	app.Name = "fossa-cli"
	app.Usage = "get dependencies from your code"
	app.Action = MakeCmd
	app.Flags = []cli.Flag{}

	app.Commands = []cli.Command{
		{
			Name:    "build",
			Aliases: []string{},
			Usage:   "discover dependencies for an inline module",
			Action:  BuildCmd,
			Flags: []cli.Flag{
				cli.StringFlag{Name: "type, t"},
				cli.BoolFlag{Name: "install, i"},
				cli.BoolFlag{Name: "no-cache"},
			},
		},
	}

	app.Run(os.Args)
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
	mod := build.Module{
		Type: c.String("type"),
	}

	buildOpts := make(map[string]interface{})
	buildOpts["install"] = c.Bool("install")
	buildOpts["no-cache"] = c.Bool("no-cache")
	if err := mod.Analyze(buildOpts); err != nil {
		log.Log.Fatalf("analysis failed (%v);\ntry pre-building and then running `fossa`", err)
	}

	log.Log.Debugf("found (%s) deduped dependencies", len(mod.Build.Dependencies))

	dat, _ := json.Marshal(mod)
	fmt.Print(string(dat))
	return nil
}

package main

import (
	config "github.com/fossas/fossa-cli/config"
	logging "github.com/op/go-logging"
	"github.com/urfave/cli"
)

var initLogger = logging.MustGetLogger("init")

func initCmd(c *cli.Context) {
	conf, err := config.Initialize(c)
	if err != nil {
		initLogger.Fatalf("Could not load configuration: %s", err.Error())
	}
	doInit(conf)
}

// doInit calls DiscoverModules() on all available integrations and writes to
func doInit(conf config.CliConfig) {
	return
}

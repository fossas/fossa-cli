package scala

import (
	"fmt"
	"os"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/mitchellh/mapstructure"
)

type Analyzer struct {
	SBTCmd     string
	SBTVersion string

	JavaCmd     string
	JavaVersion string

	Options Options
}

type Options struct{}

func New(opts map[string]interface{}) (*Analyzer, error) {
	// Set Java context variables
	javaCmd, javaVersion, err := exec.Which("-version", os.Getenv("JAVA_BINARY"), "java")
	if err != nil {
		log.Logger.Warningf("Could not find Java binary (try setting $JAVA_BINARY): %s", err.Error())
	}

	// Set SBT context variables
	sbtCmd, sbtVersion, err := exec.Which("-no-colors about", os.Getenv("SBT_BINARY"), "sbt")
	if err != nil {
		return nil, fmt.Errorf("could not find SBT binary (try setting $SBT_BINARY): %s", err.Error())
	}

	// Parse and validate options.
	var options Options
	err = mapstructure.Decode(opts, &options)
	if err != nil {
		return nil, err
	}
	log.Logger.Debug("Decoded options: %#v", options)

	analyzer := Analyzer{
		JavaCmd:     javaCmd,
		JavaVersion: javaVersion,

		SBTCmd:     sbtCmd,
		SBTVersion: sbtVersion,

		Options: options,
	}

	log.Logger.Debugf("%#v", analyzer)
	return analyzer, nil
}

func (a *Analyzer) Discover(dir string) ([]module.Module, error) {

}

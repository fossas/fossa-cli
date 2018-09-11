// Package display implements functions for displaying output to users.
package display

import (
	"io/ioutil"
	"os"
	"time"

	"github.com/apex/log"
	"github.com/briandowns/spinner"
)

var (
	file    *os.File
	s       *spinner.Spinner
	useANSI bool
	level   log.Level
)

func init() {
	// Set up spinner.
	s = spinner.New(spinner.CharSets[11], 100*time.Millisecond)
	s.Writer = os.Stderr

	// Set up log file.
	f, err := ioutil.TempFile("", "fossa-cli.*.log")
	if err != nil {
		log.WithError(err).Warnf("could not open log file")
	}
	file = f

	// Set up log handler.
	// Always set the logging package to debug, otherwise we lose the debug
	// entries that we would write to the log file.
	log.SetLevel(log.DebugLevel)
	log.SetHandler(log.HandlerFunc(Handler))
	// TODO: we do this because we log while reading files in `config.SetContext`,
	// but we really should only read files lazily.
	level = log.InfoLevel
}

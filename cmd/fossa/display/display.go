// Package display implements functions for displaying output to users.
package display

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"time"

	"github.com/apex/log"
	"github.com/briandowns/spinner"
)

func init() {
	// Set up spinner.
	s = spinner.New(spinner.CharSets[11], 100*time.Millisecond)
	s.Writer = os.Stderr

	// Set up log file.
	f, err := ioutil.TempFile("", "fossa-cli-log-")
	if err != nil {
		log.WithError(err).Warnf("could not open log file")
	}
	file = f

	// Set up log handler.
	// Always set the logging package to debug, otherwise we lose the debug
	// entries that we would write to the log file.
	log.SetLevel(log.DebugLevel)
	log.SetHandler(log.HandlerFunc(Handler))
}

// JSON is a convenience function for printing JSON to STDOUT.
func JSON(data interface{}) (int, error) {
	msg, err := json.Marshal(data)
	if err != nil {
		return 0, err
	}
	return fmt.Println(string(msg))
}

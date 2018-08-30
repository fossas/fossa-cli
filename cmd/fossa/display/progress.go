package display

import (
	"github.com/apex/log"
	"github.com/briandowns/spinner"
)

var (
	useSpinner bool
	s          *spinner.Spinner
)

// InProgress shows a progress spinner with a message.
func InProgress(message string) {
	if useSpinner && level > log.DebugLevel {
		s.Suffix = " " + message
		s.Restart()
	}
}

// ClearProgress stops a progress spinner.
func ClearProgress() {
	s.Stop()
}

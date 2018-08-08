package display

import (
	"time"

	"github.com/briandowns/spinner"
)

var (
	useSpinner bool
	s          = spinner.New(spinner.CharSets[11], 100*time.Millisecond)
)

// InProgress shows a progress spinner with a message.
func InProgress(message string) {
	if useSpinner {
		s.Suffix = " " + message
		s.Restart()
	}
}

// ClearProgress stops a progress spinner.
func ClearProgress() {
	s.Stop()
}

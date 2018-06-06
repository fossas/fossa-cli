package log

import (
	"time"

	"github.com/briandowns/spinner"
)

var (
	useSpinner bool
	s          = spinner.New(spinner.CharSets[11], 100*time.Millisecond)
)

// ShowSpinner shows a progress spinner with a message.
func ShowSpinner(message string) {
	if useSpinner {
		s.Suffix = " " + message
		s.Restart()
	}
}

// PauseSpinner pauses the spinner and returns a function for unpausing.
// TODO: we need to write our own logging functions that use this to prevent
// interrupting the spinner while logging a message.
func PauseSpinner() func() {
	s.Stop()
	return s.Restart
}

// StopSpinner stops a progress spinner.
func StopSpinner() {
	s.Stop()
}

package setup

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

// StopSpinner stops a progress spinner.
func StopSpinner() {
	s.Stop()
}

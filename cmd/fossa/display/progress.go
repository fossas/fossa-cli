package display

import (
	"github.com/apex/log"
)

// InProgress shows a progress spinner with a message.
func InProgress(message string) {
	if useANSI && level > log.DebugLevel {
		s.Suffix = " " + message
		s.Restart()
	} else {
		log.Infof(message)
	}
}

// ClearProgress stops a progress spinner.
func ClearProgress() {
	s.Stop()
}

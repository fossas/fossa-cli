package common

import (
	"fmt"
	"os"
	"time"

	"github.com/briandowns/spinner"
	logging "github.com/op/go-logging"
)

type logImpl struct {
	logger  *logging.Logger
	spinner *spinnerWrapper
}

var loggerCount = 0

func newLogService(interactive, debug bool) LogService {
	loggerCount++
	loggerName := fmt.Sprintf("fossa-cli-%d", loggerCount)
	logger := logging.MustGetLogger(loggerName)

	// If `interactive`, then use ANSI codes (spinner + colors)
	var colorOn, colorOff string
	s := &spinnerWrapper{spinner: nil}
	if interactive {
		colorOn = "%{color}"
		colorOff = "%{color:reset}"
		s = &spinnerWrapper{spinner: spinner.New(spinner.CharSets[11], 100*time.Millisecond)}
	}

	// If `debug`, then log in debug format and at debug level.
	formatter := logging.MustStringFormatter(colorOn + "%{level}" + colorOff + " %{message}")
	if debug {
		formatter = logging.MustStringFormatter(colorOn + "%{time} %{level} %{shortpkg}/%{shortfile}/%{shortfunc}" + colorOff + " %{message}")
	}
	stderrBackend := logging.AddModuleLevel(logging.NewBackendFormatter(logging.NewLogBackend(os.Stderr, "", 0), formatter))
	if debug {
		stderrBackend.SetLevel(logging.DEBUG, "")
	} else {
		stderrBackend.SetLevel(logging.WARNING, "")
	}
	logging.SetBackend(stderrBackend)

	return logImpl{logger: logger, spinner: s}
}

type spinnerWrapper struct {
	spinner *spinner.Spinner
}

func (s *spinnerWrapper) Spin(message string) {
	if s.spinner == nil {
		return
	}
	s.spinner.Suffix = " " + message
	s.spinner.Restart()
}

func (s *spinnerWrapper) Stop() {
	if s.spinner == nil {
		return
	}
	s.spinner.Stop()
}

func (logService logImpl) Spinner() Spinner {
	return logService.spinner
}

func (logService logImpl) Debugf(format string, args ...interface{}) {
	logService.logger.Debugf(format, args...)
}

func (logService logImpl) Noticef(format string, args ...interface{}) {
	logService.logger.Noticef(format, args...)
}

func (logService logImpl) Warningf(format string, args ...interface{}) {
	logService.logger.Warningf(format, args...)
}

func (logService logImpl) Fatalf(format string, args ...interface{}) {
	logService.logger.Fatalf(format, args...)
}

func (logService logImpl) Printf(format string, args ...interface{}) {
	fmt.Printf(format, args...)
}

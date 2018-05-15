package log

import (
	"fmt"
	"os"

	logging "github.com/op/go-logging"
)

var Logger = logging.MustGetLogger("fossa-cli")

var useSpinner bool

// Initialize sets up logging modes.
func Initialize(interactive, debug bool) {
	// If `interactive`, then use ANSI codes (spinner + colors)
	useSpinner = interactive
	var colorOn, colorOff string
	if interactive {
		colorOn = "%{color}"
		colorOff = "%{color:reset}"
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
}

// Print outputs a literal message to STDOUT.
func Print(args ...interface{}) {
	fmt.Print(args...)
}

// Printf outputs a formatted message to STDOUT.
func Printf(format string, args ...interface{}) {
	fmt.Printf(format, args...)
}

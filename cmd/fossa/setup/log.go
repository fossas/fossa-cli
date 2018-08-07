package setup

import (
	"io"
	"os"
	"runtime"

	logging "github.com/op/go-logging"
)

func SetLogging(interactive, debug bool, output io.Writer) {}

func Handler() {}

// TODO: we want to selectively turn logging off for tests, or maybe redirect
// test output to a buffer that we only print if `t.Failed()`, so we avoid
// dumping logs for all tests when only one test in a suite fails.

// Init initializes application-level logging.
//
// If `interactive` is true, then logging will include colors and ANSI codes
// (e.g. progress spinners). If `debug` is true, then logging will include
// debugging output.
func Init(interactive, debug bool) {
	interactive = interactive && runtime.GOOS != "windows"

	// If `interactive`, then use ANSI codes (spinner + colors)
	useSpinner = interactive
	s.Writer = os.Stderr
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

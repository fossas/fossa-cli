package display

import (
	"encoding/json"
	"fmt"
	"os"
	"runtime"

	"github.com/apex/log"
)

var (
	file     *os.File
	useColor bool
	level    log.Level
)

// SetInteractive turns colors and ANSI control characters on or off.
func SetInteractive(interactive bool) {
	// Disable Unicode and ANSI control characters on Windows.
	if runtime.GOOS == "windows" {
		return
	}

	// Configure spinner.
	useSpinner = interactive
}

// SetDebug turns debug logging to STDERR on or off.
//
// The log file always writes debug-level entries.
func SetDebug(debug bool) {
	// This sets the `level` variable rather than calling `log.SetLevel`, because
	// calling `log.SetLevel` filters entries by level _before_ they reach the
	// handler. This is not desirable, because we always want our handler to see
	// debug entries so they can be written to the log file.
	if debug {
		level = log.DebugLevel
	} else {
		level = log.InfoLevel
	}
}

// SetFile sets the log file. By default, this is set to a temporary file.
func SetFile(filename string) error {
	f, err := os.Open(filename)
	if err != nil {
		return err
	}
	file = f
	return nil
}

// File returns the log file name.
func File() string {
	return file.Name()
}

// Handler handles log entries. It multiplexes them into two outputs, writing
// human-readable messages to STDERR and machine-readable entries to a log file.
//
// TODO: does this need to be synchronised?
func Handler(entry *log.Entry) error {
	// If in debug mode, add caller.
	// TODO: implement this.

	// Write entry to STDERR.
	if entry.Level > level {
		fmt.Fprintf(os.Stderr, "%s %s\n", entry.Level, entry.Message)
	}

	// Write entry to log file.
	data, err := json.Marshal(entry)
	if err != nil {
		return err
	}
	data = append(data, byte('\n'))
	_, err = file.Write(data)
	if err != nil {
		return err
	}
	err = file.Sync()
	if err != nil {
		return err
	}

	return nil
}

// TODO: we want to selectively turn logging off for tests, or maybe redirect
// test output to a buffer that we only print if `t.Failed()`, so we avoid
// dumping logs for all tests when only one test in a suite fails.

// Init initializes application-level logging.
//
// If `interactive` is true, then logging will include colors and ANSI codes
// (e.g. progress spinners). If `debug` is true, then logging will include
// debugging output.
// func Init(interactive, debug bool) {
// 	interactive = interactive && runtime.GOOS != "windows"

// 	// If `interactive`, then use ANSI codes (spinner + colors)
// 	useSpinner = interactive
// 	s.Writer = os.Stderr
// 	var colorOn, colorOff string
// 	if interactive {
// 		colorOn = "%{color}"
// 		colorOff = "%{color:reset}"
// 	}

// 	// If `debug`, then log in debug format and at debug level.
// 	formatter := logging.MustStringFormatter(colorOn + "%{level}" + colorOff + " %{message}")
// 	if debug {
// 		formatter = logging.MustStringFormatter(colorOn + "%{time} %{level} %{shortpkg}/%{shortfile}/%{shortfunc}" + colorOff + " %{message}")
// 	}
// 	stderrBackend := logging.AddModuleLevel(logging.NewBackendFormatter(logging.NewLogBackend(os.Stderr, "", 0), formatter))
// 	if debug {
// 		stderrBackend.SetLevel(logging.DEBUG, "")
// 	} else {
// 		stderrBackend.SetLevel(logging.WARNING, "")
// 	}
// 	logging.SetBackend(stderrBackend)
// }

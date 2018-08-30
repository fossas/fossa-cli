package display

import (
	"encoding/json"
	"os"
	"runtime"
	"strconv"

	"github.com/apex/log"
	"github.com/apex/log/handlers/cli"
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
	if level == log.DebugLevel {
		_, filename, line, _ := runtime.Caller(5)
		entry.Fields["caller"] = filename + ":" + strconv.Itoa(line)
	}

	// Write entry to STDERR.
	if entry.Level >= level {
		err := cli.Default.HandleLog(entry)
		if err != nil {
			return err
		}
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

package display

import (
	"encoding/json"
	"fmt"
	"os"
	"runtime"
	"strconv"
	"strings"

	"github.com/apex/log"
	"github.com/fatih/color"
)

// SetInteractive turns colors and ANSI control characters on or off.
func SetInteractive(interactive bool) {
	// Disable Unicode and ANSI control characters on Windows.
	if runtime.GOOS == "windows" {
		useANSI = false
	} else {
		useANSI = interactive
	}
	// Use color when interactive.
	color.NoColor = !useANSI
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
		entry.Fields["callers"] = []string{}
		// See https://golang.org/pkg/runtime/#Frames
		pcs := make([]uintptr, 20)
		n := runtime.Callers(0, pcs)
		pcs = pcs[:n]
		frames := runtime.CallersFrames(pcs)
		for {
			frame, more := frames.Next()
			if !strings.Contains(frame.File, "runtime/") &&
				!strings.Contains(frame.File, "cmd/fossa/display/") &&
				!strings.Contains(frame.File, "apex/log/") {
				entry.Fields["callers"] = append(entry.Fields["callers"].([]string), frame.File+":"+frame.Function+":"+strconv.Itoa(frame.Line))
			}
			if !more {
				break
			}
		}
	}

	// Write entry to STDERR.
	if entry.Level >= level {
		msg := ""
		switch entry.Level {
		case log.DebugLevel:
			msg += color.WhiteString("DEBUG")
		case log.InfoLevel:
			msg += color.WhiteString("INFO")
		case log.WarnLevel:
			msg += color.YellowString("WARNING")
		case log.ErrorLevel:
			msg += color.RedString("ERROR")
		case log.FatalLevel:
			msg += color.RedString("FATAL")
		}

		msg += " " + entry.Message
		for _, field := range entry.Fields.Names() {
			msg += fmt.Sprintf(" %s=%+v", field, entry.Fields.Get(field))
		}

		_, err := fmt.Fprintln(os.Stderr, msg)
		if err != nil {
			return err
		}
	}

	// Write entry to log file.
	data, err := json.Marshal(entry)
	if err != nil {
		// There are some entries that we can't serialize to JSON (for example, a
		// map that uses a struct as a key). In these cases, serialize to Go format
		// and print a string.
		switch err.(type) {
		case *json.UnsupportedTypeError:
			data = []byte(fmt.Sprintf("%#v", entry))
		case *json.UnsupportedValueError:
			data = []byte(fmt.Sprintf("%#v", entry))
		default:
			return err
		}
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

package log

import (
	"fmt"

	logging "github.com/op/go-logging"
)

var logger = logging.MustGetLogger("fossa-cli")

// Debug outputs a literal debug message. Debug messages are useful for tracing
// execution and diagnosing unintended error cases.
func Debug(args ...interface{}) {
	Debugf("%s", fmt.Sprint(args...))
}

// Debugf outputs a formatted debug message.
func Debugf(format string, args ...interface{}) {
	logger.Debugf(format, args...)
}

// Notice outputs a literal notice message. Notices are non-error events that
// the user should be informed of.
func Notice(args ...interface{}) {
	Noticef("%s", fmt.Sprint(args...))
}

// Noticef outputs a formatted notice message.
func Noticef(format string, args ...interface{}) {
	logger.Noticef(format, args...)
}

// Warning outputs a literal warning message. Warnings are non-fatal error
// events.
func Warning(args ...interface{}) {
	Warningf("%s", fmt.Sprint(args...))
}

// Warningf outputs a formatted warning message.
func Warningf(format string, args ...interface{}) {
	logger.Warningf(format, args...)
}

// Fatal outputs a literal error message. Fatal errors are non-recoverable, and
// cause an `os.Exit(1)`.
func Fatal(args ...interface{}) {
	Fatalf("%s", fmt.Sprint(args...))
}

// Fatalf outputs a formatted error message.
func Fatalf(format string, args ...interface{}) {
	logger.Fatalf(format, args...)
}

// Panicf outputs a panic. Panics are errors that should never happen and
// indicate that something has gone terribly wrong. They are akin to assertion
// failures.
func Panicf(format string, args ...interface{}) {
	logger.Panicf(format, args...)
}

package errors

import (
	"fmt"

	"github.com/fatih/color"
	"github.com/pkg/errors"
)

// General errors.
var (
	ErrNotImplemented = errors.New("not yet implemented")
)

type Type = int

const (
	User Type = iota
	Exec
	Unknown
)

// UnknownError creates a simple fossa error using an existing error and additional context.
func UnknownError(err error, message string) *Error {
	return &Error{
		Cause: err,
		Type:  Unknown,
	}
}

// Error is the fossa implementation of errors for providing user-friendly information.
type Error struct {
	ExitCode        int
	Cause           error  // Base error.
	Type            Type   // Type helps us tell the user to log an issue, go to docs, etc.
	Message         string // Help message for the user, contact support, opening an issue, etc.
	Troubleshooting string // Simple solution or debugging instructions.
	Link            string // Link to documentation or reference information.
}

func (e *Error) Error() string {
	var err, code, troubleshooting, link, message string

	if e.Cause != nil {
		err = e.Cause.Error()
	}

	if e.ExitCode != 0 {
		code = fmt.Sprintf("\n%s: %d", color.BlueString("EXIT CODE"), e.ExitCode)
	}

	if e.Troubleshooting != "" {
		troubleshooting = fmt.Sprintf("\n%s: %s", color.MagentaString("TROUBLESHOOTING"), e.Troubleshooting)
	}

	if e.Link != "" {
		link = fmt.Sprintf("\n%s: %s", color.GreenString("LINK"), e.Link)
	}

	message = e.Message
	if message == "" {
		switch e.Type {
		case User:
		case Exec:
			fallthrough
		case Unknown:
			fallthrough
		default:
			message = ReportBugMessage
		}
	}

	return err + code + troubleshooting + link + message
}

func Errorf(format string, args ...interface{}) error {
	return errors.Errorf(format, args...)
}

func Wrap(cause error, msg string) error {
	return errors.Wrap(cause, msg)
}

func Wrapf(cause error, format string, args ...interface{}) error {
	return errors.Wrapf(cause, format, args...)
}

func WrapError(cause error, err Error) Error {
	switch e := cause.(type) {
	case *Error:
		return Error{
			Cause:           e,
			Message:         err.Message,
			Troubleshooting: err.Troubleshooting,
		}
	default:
	}
	return err
}

func New(msg string) error {
	return errors.New(msg)
}

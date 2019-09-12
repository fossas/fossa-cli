package errors

import (
	"fmt"

	"github.com/fatih/color"
	"github.com/mitchellh/go-wordwrap"
	"github.com/pkg/errors"
)

type Type = int

const (
	User Type = iota
	Exec
	Unknown
	NotImplemented
)

// UnknownError creates a simple fossa error using an existing error and additional context.
func UnknownError(err error, message string) *Error {
	return &Error{
		Cause:           err,
		Type:            Unknown,
		Troubleshooting: message,
	}
}

// NotImplemented should be used to signify that a code path is not yet implemented.
func NotImplementedError() *Error {
	return &Error{
		Type: NotImplemented,
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
	if e == nil {
		return typeNilError
	}

	if e.Cause != nil {
		err = fmt.Sprintf("%s: %s", color.RedString("ERROR"), e.Cause.Error())
	}

	if e.ExitCode != 0 {
		code = fmt.Sprintf("\n%s: %d", color.BlueString("EXIT CODE"), e.ExitCode)
	}

	if e.Troubleshooting != "" {
		troubleshooting = wordwrap.WrapString(fmt.Sprintf("\n%s: %s", color.MagentaString("TROUBLESHOOTING"), e.Troubleshooting), width)
	}

	if e.Link != "" {
		link = fmt.Sprintf("\n%s: %s", color.GreenString("LINK"), e.Link)
	}

	message = e.Message
	if message == "" {
		switch e.Type {
		case NotImplemented:
			message = NotImplementedMessage
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

func (err *Error) WrapCause(msg string) *Error {
	if err.Cause == nil {
		err.Cause = errors.New(msg)
	} else {
		err.Cause = Wrap(err.Cause, msg)
	}
	return err
}

func New(msg string) error {
	return errors.New(msg)
}

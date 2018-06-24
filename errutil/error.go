package errutil

import (
	"strings"
)

// TODO: we need to refactor all application-level errors to this struct.

// Error is a custom application-level error primitive, that supports our
// particular use cases for user error message reporting.
type Error struct {
	Message string
	Fields  map[string]string
	Cause   error
}

// Error implements error for Error.
func (e *Error) Error() string {
	msg := e.Message
	if e.Fields != nil {
		msg += "("
		var fields []string
		for k, v := range e.Fields {
			fields = append(fields, k+": "+v)
		}
		msg += strings.Join(fields, ", ")
		msg += ")"
	}
	if e.Cause != nil {
		msg += ": " + e.Cause.Error()
	}
	return msg
}

// Is defines comparison for error messages on the Message field. This is useful
// for comparing against sentinels, for the sake of providing more verbose and
// useful error messages when reporting to the user.
func (e *Error) Is(other Error) bool {
	return e.Message == other.Message
}

func (e *Error) Field(key, value string) *Error {
	if e.Fields == nil {
		e.Fields = make(map[string]string)
	}
	e.Fields[key] = value
	return e
}

// New implements errors.New for Error.
func New(message string) Error {
	return Error{Message: message}
}

// Wrap implements errors.Wrap for Error.
func Wrap(err error, message string) Error {
	return Error{Message: message, Cause: err}
}

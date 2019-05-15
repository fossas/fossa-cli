package errors

import "github.com/pkg/errors"

// General errors.
var (
	ErrNotImplemented = errors.New("not yet implemented")
)

// Error is the fossa implementation of errors for providing user-friendly information.
type Error struct {
	ExitCode        int
	Cause           error
	Type            string
	Message         string
	Troubleshooting string
}

func (e *Error) Error() string {
	return `Error: ` + e.Cause.Error() + `
TROUBLESHOOTING:

` + e.Troubleshooting + `

Please try troubleshooting before filing a bug. If none of the suggestions work,
you can file a bug at <https://github.com/fossas/fossa-cli/issues/new>.

For additional support send an email to support@fossa.com with as much information
as possible, screenshots are greatly appreciated!

CREATING AN ISSUE:

Before creating an issue, please search GitHub issues for similar problems. When
creating the issue, please attach the debug log located at:

  /tmp/fossa-cli-debug-log
`
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

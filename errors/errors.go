package errors

import "github.com/pkg/errors"

// General errors.
var (
	ErrNotImplemented = errors.New("not yet implemented")
)

type Error struct {
	Cause        error
	Common       bool
	Explanation  string
	Instructions []string
}

func (e *Error) Error() string {
	return `Error: unable to foo the bar.

TROUBLESHOOTING:

This error is commonly seen. It usually occurs because mvn dependency:list is failing.

- Ensure that mvn dependency:list works

Please try the suggestions before filing a bug. If none of the suggestions work,
you can file a bug at <https://github.com/fossas/fossa-cli/issues/new>.

For additional support, ask the #cli channel at <https://slack.fossa.io>.

CREATING AN ISSUE:

Before creating an issue, please search GitHub issues for similar problems. When
creating the issue, please attach the debug log located at:
	/tmp/fossa-cli-debug-log.`
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
			Cause:        e,
			Common:       err.Common,
			Explanation:  err.Explanation,
			Instructions: err.Instructions,
		}
	default:
	}
	return err
}

func New(msg string) error {
	return errors.New(msg)
}

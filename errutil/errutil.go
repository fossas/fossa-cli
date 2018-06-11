// Package errutil contains common application-level errors. They reside here
// to avoid cyclic dependencies.
package errutil

import "errors"

// General errors.
var (
	ErrNotImplemented = errors.New("not yet implemented")
)

// Analysis errors.
var (
	ErrNoRevisionForPackage = errors.New("no revision found for package name")
)

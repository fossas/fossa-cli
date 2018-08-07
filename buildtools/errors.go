package buildtools

import "errors"

// Analysis errors.
var (
	ErrNoRevisionForPackage = errors.New("no revision found for package name")
)

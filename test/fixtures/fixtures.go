package fixtures

import (
	"errors"
	"path/filepath"
	"runtime"
)

func getFixtureFolder() (string, error) {
	_, filename, _, ok := runtime.Caller(1)
	if !ok {
		return "", errors.New("could not load fixture path")
	}
	return filepath.Dir(filename), nil
}

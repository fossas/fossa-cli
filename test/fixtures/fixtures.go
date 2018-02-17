package fixtures

import (
	"fmt"
	"path/filepath"
	"runtime"
)

func getFixtureFolder() (string, error) {
	_, filename, _, ok := runtime.Caller(1)
	if !ok {
		return "", fmt.Errorf("could not load fixture path")
	}
	return filepath.Dir(filename), nil
}

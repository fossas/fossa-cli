package golang

import (
	"strings"
)

// Dir returns the absolute path to a Go package.
func (a *Analyzer) Dir(importpath string) (string, error) {
	pkg, err := a.Go.ListOne(importpath)
	if err != nil {
		return "", err
	}
	return pkg.Dir, nil
}

// Unvendor takes a vendorized import path and strips all vendor folder
// prefixes.
func Unvendor(importpath string) string {
	sections := strings.Split(importpath, "/vendor/")
	return sections[len(sections)-1]
}

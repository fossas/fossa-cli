package golang

// Dir returns the absolute path to a Go package.
func (a *Analyzer) Dir(importpath string) (string, error) {
	pkg, err := a.Go.ListOne(importpath)
	if err != nil {
		return "", err
	}
	return pkg.Dir, nil
}

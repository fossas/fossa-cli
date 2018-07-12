package fossa

import "github.com/fossas/fossa-cli/errutil"

// A Build holds the FOSSA API response for the builds API.
type Build struct {
	ID    int
	Error string
	Task  struct {
		Status string
	}
}

// GetBuild loads the build for a project.
func GetBuild(fetcher, project, revision string) (Build, error) {
	return Build{}, errutil.ErrNotImplemented
}

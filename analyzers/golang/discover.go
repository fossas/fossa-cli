package golang

import (
	"os"
	"path/filepath"

	"github.com/apex/log"
	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/buildtools/gocmd"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type DiscoverOptions struct {
	Strategy string

	Cmd       string
	BuildOS   string
	BuildArch string
}

// Discover runs `go list ./...`.
func Discover(dir string, opts map[string]interface{}) ([]module.Module, error) {
	log.Debugf("%#v", opts)

	// Parse and validate options.
	var options DiscoverOptions
	err := mapstructure.Decode(opts, &options)
	if err != nil {
		return nil, err
	}
	log.WithField("options", options).Debug("parsed analyzer options")

	cmd, _, err := exec.Which("version", options.Cmd, "go")
	if err != nil {
		return nil, errors.New("`go` not found, skipping searching for Go projects")
	}

	g := gocmd.Go{
		Cmd: cmd,
		Dir: dir,
	}
	found, err := g.List([]string{"./..."}, nil)
	if err != nil {
		return nil, errors.Wrap(err, "could not find Go projects")
	}

	cwd, err := os.Getwd()
	if err != nil {
		return nil, errors.Wrap(err, "could not find Go projects")
	}
	var projects []module.Module
	for _, f := range found {
		log.Debugf("Found Go module: %#v", f)
		path, err := filepath.Rel(cwd, f.Dir)
		if err != nil {
			return nil, errors.Wrap(err, "could not find Go projects")
		}
		projects = append(projects, module.Module{
			Name:         Unvendor(f.ImportPath),
			Type:         pkg.Go,
			IsExecutable: f.Name == "main",
			BuildTarget:  f.ImportPath,
			Dir:          path,
		})
	}
	return projects, nil
}

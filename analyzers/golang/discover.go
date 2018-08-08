package golang

import (
	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/buildtools/gocmd"
	"github.com/fossas/fossa-cli/exec"
	"github.com/apex/log"
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
	log.Logger.Debug("%#v", opts)

	// Parse and validate options.
	var options DiscoverOptions
	err := mapstructure.Decode(opts, &options)
	if err != nil {
		return nil, err
	}
	log.Logger.Debug("Decoded options: %#v", options)

	cmd, _, err := exec.Which("version", options.Cmd, "go")
	if err != nil {
		return nil, errors.New("`go` not found, skipping searching for Go projects")
	}

	g := gocmd.Go{
		Cmd:  cmd,
		OS:   options.BuildOS,
		Arch: options.BuildArch,
	}
	found, err := g.List([]string{"./..."})
	if err != nil {
		return nil, errors.Wrap(err, "could not find Go projects")
	}

	var projects []module.Module
	for _, f := range found {
		log.Logger.Debugf("Found Go module: %#v", f)
		projects = append(projects, module.Module{
			Name:         Unvendor(f.ImportPath),
			Type:         pkg.Go,
			IsExecutable: f.Name == "main",
			BuildTarget:  f.ImportPath,
			Dir:          f.Dir,
		})
	}
	return projects, nil
}

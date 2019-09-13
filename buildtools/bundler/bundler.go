package bundler

import (
	"fmt"
	"os"
	"strings"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type Bundler struct {
	Cmd string
}

type Gem struct {
	Name     string
	Revision string
}

func New() (Bundler, *errors.Error) {
	bundlerCmd, _, err := exec.Which("--version", os.Getenv("FOSSA_BUNDLER_CMD"), "bundler", "bundle")
	if err == nil {
		return Bundler{}, &errors.Error{
			Cause:           err,
			Type:            errors.Exec,
			Troubleshooting: "FOSSA uses bundler to analyze Ruby projects but was unable to find the command. Ensure that `bundle` is accessibly in the environment you are running the FOSSA CLI in. If you are using a custom bundle command set the environment variable FOSSA_BUNDLER_CMD to its location.",
			Link:            "bundler.io/man/bundle-list.1.html",
		}
	}
	return Bundler{Cmd: bundlerCmd}, nil
}

func (b *Bundler) ListLockfileGraph(lockfilePath string) (graph.Deps, *errors.Error) {
	lockfile, err := FromLockfile(lockfilePath)
	if err != nil {
		return graph.Deps{}, err
	}

	gems, err := b.List()
	if err != nil {
		return graph.Deps{}, err
	}

	imports, deps := FilteredLockfile(gems, lockfile)

	return graph.Deps{
		Direct:     imports,
		Transitive: deps,
	}, nil
}

func (b *Bundler) ListGraph() (graph.Deps, *errors.Error) {
	gems, err := b.List()
	if err != nil {
		return graph.Deps{}, err
	}

	return graphFromGems(gems), nil
}

func graphFromGems(gems []Gem) graph.Deps {
	depGraph := graph.Deps{Transitive: make(map[pkg.ID]pkg.Package)}
	for _, gem := range gems {
		id := pkg.ID{
			Type:     pkg.Ruby,
			Name:     gem.Name,
			Revision: gem.Revision,
		}

		depGraph.Direct = append(depGraph.Direct, pkg.Import{Resolved: id})
		depGraph.Transitive[id] = pkg.Package{ID: id}
	}

	return depGraph
}

func (b *Bundler) List() ([]Gem, *errors.Error) {
	stdout, stderr, err := exec.Run(exec.Cmd{
		Name: b.Cmd,
		Argv: []string{"list"},
	})
	if err != nil {
		return nil, &errors.Error{
			Cause:           err,
			Type:            errors.Exec,
			Troubleshooting: fmt.Sprintf("Fossa could not run `%s list` within the current directory. Try running this command and ensure that %s is installed in your environment.\nstdout: %s\nstderr: %s", b.Cmd, b.Cmd, stdout, stderr),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/ruby.md#support",
		}
	}
	contents := strings.TrimSpace(strings.TrimPrefix(stdout, "Gems included by the bundle:\n"))
	lines := strings.Split(contents, "\n")
	var gems []Gem
	for _, line := range lines[1:] { // The first line is "Gems included by the bundle:"
		trimmed := strings.TrimPrefix(line, "  * ")
		log.WithField("trimmed", trimmed).Debug("parsing line")
		sections := strings.Split(trimmed, " ")
		gems = append(gems, Gem{
			Name:     sections[0],
			Revision: sections[1],
		})
	}
	return gems, nil
}

func (b *Bundler) Install(flags ...string) error {
	if flags == nil {
		flags = []string{"--frozen", "--deployment"}
	}
	stdout, stderr, err := exec.Run(exec.Cmd{
		Name: b.Cmd,
		Argv: append([]string{"install"}, flags...),
	})
	if err != nil {
		return &errors.Error{
			Cause:           err,
			Type:            errors.Exec,
			Troubleshooting: fmt.Sprintf("Fossa could not run `%s install --frozen --deployment` within the current directory. Try running this command and ensure that %s is installed in your environment.\nstdout: %s\nstderr: %s", b.Cmd, b.Cmd, stdout, stderr),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/ruby.md#support",
		}

	}
	return nil
}

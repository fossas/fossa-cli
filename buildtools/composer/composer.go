package composer

import (
	"encoding/json"
	"regexp"
	"strings"

	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/log"
)

type Composer struct {
	Cmd string
}

type Package struct {
	Name        string
	Version     string
	Description string
}

type Show struct {
	Installed []Package
}

func (c *Composer) Dependencies(dir string) ([]Package, map[Package][]Package, error) {
	// Run `composer show --format=json --no-ansi` to get resolved versions
	show, err := c.Show(dir)
	if err != nil {
		return nil, nil, err
	}

	pkgMap := make(map[string]Package)
	for _, dep := range show.Installed {
		pkgMap[dep.Name] = dep
	}

	// Run `composer show --tree --no-ansi` to get paths
	treeOutput, _, err := exec.Run(exec.Cmd{
		Name: c.Cmd,
		Argv: []string{"show", "--tree", "--no-ansi"},
		Dir:  dir,
	})
	if err != nil {
		return nil, nil, errors.Wrap(err, "could not get dependency list from Composer")
	}

	result, err := graph.ReadTree(treeOutput, func(line string) (int, interface{}, error) {
		// Skip empty lines.
		if line == "" {
			return -1, nil, graph.ErrSkipLine
		}

		if line[0] != '`' && line[0] != '|' && line[0] != ' ' {
			// We're at a top-level package.
			sections := strings.Split(line, " ")
			name := sections[0]
			return 1, pkgMap[name], nil
		}

		// We're somewhere in the tree.
		r := regexp.MustCompile("^([ \\|`-]+)([^ \\|`-][^ ]+) (.*)$")
		matches := r.FindStringSubmatch(line)
		name := matches[2]
		depth := len(matches[1])
		if depth%3 != 0 {
			// Sanity check
			log.Logger.Panicf("Bad depth: %#v %s %#v", depth, line, matches)
		}

		// Skip PHP runtime dependencies (these are always leaves)
		if matches[2] == "php" {
			return -1, nil, graph.ErrSkipLine
		}

		return depth/3 + 1, pkgMap[name], nil
	})
	if err != nil {
		return nil, nil, err
	}

	var imports []Package
	g := make(map[Package][]Package)

	err = graph.Unwrap(&imports, &g, result)
	if err != nil {
		return nil, nil, err
	}
	return imports, g, nil
}

func (c *Composer) Show(dir string) (Show, error) {
	// Run `composer show --format=json --no-ansi` to get resolved versions
	showOutput, _, err := exec.Run(exec.Cmd{
		Name: c.Cmd,
		Argv: []string{"show", "--format=json", "--no-ansi"},
		Dir:  dir,
	})
	if err != nil {
		return Show{}, errors.Wrap(err, "could not get dependency list from Composer")
	}
	var showJSON Show
	err = json.Unmarshal([]byte(showOutput), &showJSON)
	if err != nil {
		return Show{}, errors.Wrapf(err, "could not parse dependency list as JSON: %#v", showOutput)
	}
	return showJSON, nil
}

func (c *Composer) Clean(dir string) error {
	return files.Rm(dir, "vendor")
}

func (c *Composer) Install(dir string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: c.Cmd,
		Argv: []string{"install", "--prefer-dist", "--no-dev", "--no-plugins", "--no-scripts"},
		Dir:  dir,
	})
	return err
}

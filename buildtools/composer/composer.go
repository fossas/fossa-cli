package composer

import (
	"encoding/json"
	"regexp"
	"strings"

	"github.com/apex/log"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
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

//go:generate bash -c "genny -in=$GOPATH/src/github.com/fossas/fossa-cli/graph/readtree.go gen 'Generic=Package' | sed -e 's/package graph/package composer/' > readtree_generated.go"

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

	// Skip empty lines.
	var filteredLines []string
	for _, line := range strings.Split(treeOutput, "\n") {
		if line != "" {
			filteredLines = append(filteredLines, line)
		}
	}

	return ReadPackageTree(filteredLines, func(line string) (int, Package, error) {
		if line[0] != '`' && line[0] != '|' && line[0] != ' ' {
			// We're at a top-level package.
			sections := strings.Split(line, " ")
			name := sections[0]
			log.Logger.Debugf("DIRECT %#v", name)
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
		level := depth/3 + 1
		log.Logger.Debugf("%#v %#v", level, name)

		// Resolve special names
		if name == "php" || strings.HasPrefix(name, "ext-") {
			return level, Package{Name: name}, nil
		}

		p, ok := pkgMap[name]
		if !ok {
			log.Logger.Warningf("Could not resolve: %#v", name)
			return level, Package{Name: name}, nil
		}
		return level, p, nil
	})
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

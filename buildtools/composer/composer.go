package composer

import (
	"encoding/json"
	"regexp"
	"strings"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
)

//go:generate bash -c "genny -in=$GOPATH/src/github.com/fossas/fossa-cli/graph/readtree.go gen 'Generic=Package' | sed -e 's/package graph/package composer/' > readtree_generated.go"

// A Composer can return the output of the `show` and `install` commands.
type Composer interface {
	// Show returns the output of running the `show` command in dir with optional additional arguments.
	Show(dir string, args ...string) (stdout string, stderr string, err error)

	// Install returns the output of running the `install` command in dir with optional additional arguments.
	Install(dir string, args ...string) (stdout string, stderr string, err error)
}

// NewComposer returns a Runner that invokes the real composer binary.
func NewComposer(composerBinary string) Composer {
	return runner(composerBinary)
}

type Package struct {
	Name        string
	Version     string
	Description string
}

// A ShowOutput structure has a list of dependencies reported by Composer.
type ShowOutput struct {
	Installed []Package `json:"installed"`
}

// The runner type implements the Composer interface and execs the composer binary.
type runner string

func (r runner) Show(dir string, args ...string) (stdout string, stderr string, err error) {
	return exec.Run(exec.Cmd{
		Name: string(r),
		Argv: append([]string{"show"}, args...),
		Dir:  dir,
	})
}

func (r runner) Install(dir string, args ...string) (stdout string, stderr string, err error) {
	return exec.Run(exec.Cmd{
		Name: string(r),
		Argv: append([]string{"install"}, args...),
		Dir:  dir,
	})
}

func Dependencies(dir string, c Composer) ([]Package, map[Package][]Package, error) {
	// Run `composer show --format=json --no-ansi` to get resolved versions.
	show, err := Show(dir, c)
	if err != nil {
		return nil, nil, err
	}

	pkgMap := make(map[string]Package)
	for _, dep := range show.Installed {
		pkgMap[dep.Name] = dep
	}

	// Run `composer show --tree --no-ansi` to get paths.
	treeOutput, _, err := c.Show(dir, "--tree", "--no-ansi")
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

	treeLine := regexp.MustCompile("^([ |`-]+)([^ |`-][^ ]+) .*$")

	imports, deps, err := ReadPackageTree(filteredLines, func(line string) (int, Package, error) {
		if line[0] != '`' && line[0] != '|' && line[0] != ' ' {
			// We're at a top-level package.
			sections := strings.Split(line, " ")
			name := sections[0]
			log.WithField("name", name).Debug("parsing Composer package")
			return 1, pkgMap[name], nil
		}

		// We're somewhere in the tree.
		matches := treeLine.FindStringSubmatch(line)
		name := matches[2]
		depth := len(matches[1])
		if depth%3 != 0 {
			// Sanity check
			panic(line)
		}
		level := depth/3 + 1
		log.WithFields(log.Fields{
			"name":  name,
			"level": level,
		}).Debug("parsing Composer tree")

		// Resolve special names.
		if name == "php" || strings.HasPrefix(name, "ext-") {
			return level, Package{Name: name}, nil
		}

		p, ok := pkgMap[name]
		if !ok {
			log.WithField("name", name).Warn("could not resolve Composer package version")
			return level, Package{Name: name}, nil
		}
		return level, p, nil
	})

	if err != nil {
		return imports, deps, err
	}

	// Filter out "comp+php" imports
	filteredImports := make([]Package, 0)
	for _, currPackage := range imports {
		if currPackage.Name != "php" {
			filteredImports = append(filteredImports, currPackage)
		}
	}

	// Filter out deps, both parent and children with locator as "php"
	filteredDeps := make(map[Package][]Package)
	for parent, children := range deps {
		if parent.Name != "php" {
			_, ok := filteredDeps[parent]
			if !ok {
				filteredDeps[parent] = make([]Package, 0)
			}
			for _, child := range children {
				if child.Name != "php" {
					filteredDeps[parent] = append(filteredDeps[parent], child)
				}
			}
		}
	}

	return filteredImports, filteredDeps, nil
}

// Install calls Install on the Composer with dir as the CWD.
func Install(dir string, c Composer) error {
	_, _, err := c.Install(dir, "--prefer-dist", "--no-dev", "--no-plugins", "--no-scripts")
	return err
}

// Show calls Show on the Composer with dir as the CWD.
func Show(dir string, c Composer) (ShowOutput, error) {
	// Run `composer show --format=json --no-ansi` to get resolved versions
	output, _, err := c.Show(dir, "--format=json", "--no-ansi")
	if err != nil {
		return ShowOutput{}, errors.Wrap(err, "could not get dependency list from Composer")
	}
	var showJSON ShowOutput
	// If there are no deps, `[]` will be returned. Do not attempt to unmarshal it into a Show struct.
	if strings.HasPrefix(output, "[]") {
		return showJSON, nil
	}
	err = json.Unmarshal([]byte(output), &showJSON)
	if err != nil {
		return ShowOutput{}, errors.Wrapf(err, "could not parse dependency list as JSON: %#v", output)
	}
	return showJSON, nil
}

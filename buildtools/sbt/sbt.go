package sbt

import (
	"regexp"
	"strings"

	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/log"
)

type SBT struct {
	Bin string
}

func (s *SBT) Clean(dir, project, configuration string) error {
	_, _, err := exec.Run(exec.Cmd{
		Dir:  dir,
		Name: s.Bin,
		Argv: []string{Task(project, configuration, "clean")},
	})
	return err
}

func (s *SBT) Compile(dir, project, configuration string) error {
	_, _, err := exec.Run(exec.Cmd{
		Dir:  dir,
		Name: s.Bin,
		Argv: []string{Task(project, configuration, "compile")},
	})
	return err
}

func (s *SBT) Projects(dir string) ([]string, error) {
	output, _, err := exec.Run(exec.Cmd{
		Dir:  dir,
		Name: s.Bin,
		Argv: []string{"-no-colors", "projects"},
	})
	if err != nil {
		return nil, errors.Wrap(err, "could not get projects from SBT")
	}

	// Filter lines to only include projects.
	var projectLines []string
	for _, line := range strings.Split(output, "\n") {
		if FilterLine(line) {
			log.Logger.Debugf("Matched line: %#v", line)
			projectLines = append(projectLines, line)
		} else {
			log.Logger.Debugf("Ignoring line: %#v", line)
		}
	}

	var projects []string
	for _, project := range projectLines {
		log.Logger.Debugf("Raw project: %#v", project)
		noPrefix := strings.TrimPrefix(project, "[info] 	   ")
		noSelectedPrefix := strings.TrimPrefix(noPrefix, "[info] 	 * ")
		log.Logger.Debugf("Trimmed project: %#v", project)
		projects = append(projects, noSelectedPrefix)
	}
	return projects, nil
}

//go:generate bash -c "genny -in=$GOPATH/src/github.com/fossas/fossa-cli/graph/readtree.go gen 'Generic=Dependency' | sed -e 's/package graph/package sbt/' > readtree_generated.go"

type Dependency struct {
	Name    string
	Version string
}

func (s *SBT) DependencyTree(dir, project, configuration string) ([]Dependency, map[Dependency][]Dependency, error) {
	output, _, err := exec.Run(exec.Cmd{
		Dir:  dir,
		Name: s.Bin,
		Argv: []string{"-no-colors", Task(project, configuration, "dependencyTree")},
	})
	if err != nil {
		return nil, nil, errors.Wrap(err, "could not get dependency tree from SBT")
	}

	return ParseDependencyTree(output, project == "root" || project == "")
}

func (s *SBT) DependencyList(dir, project, configuration string) (string, error) {
	output, _, err := exec.Run(exec.Cmd{
		Dir:  dir,
		Name: s.Bin,
		Argv: []string{"-no-colors", Task(project, configuration, "dependencyList")},
	})
	if err != nil {
		return "", errors.Wrap(err, "could not get dependency list from SBT")
	}

	// Filter lines to only include dependency list.
	var depLines []string
	for _, line := range strings.Split(output, "\n") {
		if FilterLine(line) {
			log.Logger.Debugf("Matched line: %#v", line)
			depLines = append(depLines, line)
		} else {
			log.Logger.Debugf("Ignoring line: %#v", line)
		}
	}
	return strings.Join(depLines, "\n"), err
}

func ParseDependencyTree(output string, rootBuild bool) ([]Dependency, map[Dependency][]Dependency, error) {
	// Filter lines to only include dependency tree.
	spacerRegex := regexp.MustCompile("^\\[info\\] ([ `+\\\\|-]*)(\\s*?)$")
	var depLines []string
	for _, line := range strings.Split(output, "\n") {
		if FilterLine(line) && !spacerRegex.MatchString(line) {
			log.Logger.Debugf("Matched line: %#v", line)
			depLines = append(depLines, line)
		} else {
			log.Logger.Debugf("Ignoring line: %#v", line)
		}
	}

	// Non-root builds only have one sub-project, so we collapse the first layer
	// of imports.
	if !rootBuild {
		// Remove the sub-project line.
		depLines = depLines[1:]
	}

	depRegex := regexp.MustCompile("^\\[info\\] ([ `+\\\\|-]*)([^ `+\\\\|-].+)$")
	locatorRegex := regexp.MustCompile("([^:\\s]+):([^:\\s]+):([^:\\s]+).*")
	return ReadDependencyTree(depLines, func(line string) (int, Dependency, error) {
		log.Logger.Debugf("Parsing line: %#v\n", line)

		// Match for context
		depMatches := depRegex.FindStringSubmatch(line)
		log.Logger.Debugf("Dep matches: %#v\n", depMatches)
		depth := len(depMatches[1])

		// SBT quirk: the indentation from level 1 to level 2 is 4 spaces, but all others are 2 spaces
		if depth >= 4 {
			depth -= 2
		}
		// Handle non-root builds.
		if !rootBuild {
			depth -= 2
		}

		if depth%2 != 0 {
			// Sanity check
			log.Logger.Panicf("Bad depth: %#v %s %#v", depth, line, depMatches)
		}
		// Parse locator
		locatorMatches := locatorRegex.FindStringSubmatch(depMatches[2])
		log.Logger.Debugf("Locator matches: %#v\n", locatorMatches)
		dep := Dependency{
			Name:    locatorMatches[1] + ":" + locatorMatches[2],
			Version: locatorMatches[3],
		}

		return depth / 2, dep, nil
	})
}

func FilterLine(line string) bool {
	trimmed := strings.TrimSpace(line)
	if !strings.HasPrefix(trimmed, "[info ]") {
		return false
	}
	infoMsg := strings.TrimPrefix(trimmed, "[info] ")
	return !(strings.HasPrefix(infoMsg, "Loading ") ||
		strings.HasPrefix(infoMsg, "Compiling ") ||
		strings.HasPrefix(infoMsg, "Non-compiled module ") ||
		strings.HasPrefix(strings.TrimSpace(infoMsg), "Compilation ") ||
		strings.HasPrefix(infoMsg, "Resolving ") ||
		strings.HasPrefix(infoMsg, "Resolved ") ||
		strings.HasPrefix(infoMsg, "Set ") ||
		strings.HasPrefix(infoMsg, "In file:") ||
		strings.HasPrefix(infoMsg, "Updating ") ||
		strings.HasPrefix(infoMsg, "Done ") ||
		strings.HasPrefix(infoMsg, "downloading ") ||
		strings.HasPrefix(strings.TrimSpace(infoMsg), "[SUCCESSFUL ]"))
}

func Task(project, configuration, task string) string {
	cmd := task
	if configuration != "" {
		cmd = configuration + ":" + cmd
	}
	if project != "" {
		cmd = project + "/" + cmd
	}
	return cmd
}

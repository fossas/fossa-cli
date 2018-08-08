package sbt

import (
	"regexp"
	"strings"

	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/apex/log"
	"github.com/fossas/fossa-cli/pkg"
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

type Dependency struct {
	Name    string
	Version string
}

func (s *SBT) DependencyTree(dir, project, configuration string) (pkg.Imports, pkg.Deps, error) {
	output, _, err := exec.Run(exec.Cmd{
		Dir:  dir,
		Name: s.Bin,
		Argv: []string{"-no-colors", Task(project, configuration, "dependencyGraphMl")},
	})
	if err != nil {
		return nil, nil, errors.Wrap(err, "could not get dependency graph from SBT")
	}

	file := ""
	r := regexp.MustCompile("^\\[info\\] Wrote dependency graph to '(.*?)'$")
	for _, line := range strings.Split(output, "\n") {
		matches := r.FindStringSubmatch(line)
		if matches != nil {
			file = matches[1]
		}
	}
	log.Logger.Debugf("file: %#v", file)

	var root GraphML
	err = files.ReadXML(&root, file)
	if err != nil {
		return nil, nil, errors.Wrap(err, "could not parse SBT dependency graph")
	}
	log.Logger.Debugf("graph: %#v", root)

	evicted, _, err := exec.Run(exec.Cmd{
		Dir:  dir,
		Name: s.Bin,
		Argv: []string{"-no-colors", Task(project, configuration, "evicted")},
	})
	if err != nil {
		return nil, nil, errors.Wrap(err, "could not get version conflict resolutions from SBT")
	}

	return ParseDependencyGraph(root.Graph, evicted)
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

func FilterLine(line string) bool {
	trimmed := strings.TrimSpace(line)
	if !strings.HasPrefix(trimmed, "[info] ") {
		return false
	}
	infoMsg := strings.TrimPrefix(trimmed, "[info] ")
	return !(strings.HasPrefix(infoMsg, "Loading ") ||
		strings.HasPrefix(infoMsg, "Compiling ") ||
		strings.HasPrefix(infoMsg, "Non-compiled module ") ||
		strings.HasPrefix(strings.TrimSpace(infoMsg), "Compilation ") ||
		strings.HasPrefix(infoMsg, "Resolving ") ||
		strings.HasPrefix(infoMsg, "Resolved ") ||
		strings.HasPrefix(infoMsg, "Fetching ") ||
		strings.HasPrefix(infoMsg, "Fetched ") ||
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

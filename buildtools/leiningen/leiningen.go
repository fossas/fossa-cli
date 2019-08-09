package leiningen

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

// Input models any
type Output interface {
	DependencyGraph(string) (graph.Deps, *errors.Error)
}

// Dependency models a leiningen dependency.
type Dependency struct {
	GroupID    string `xml:"groupId"`
	ArtifactID string `xml:"artifactId"`
	Version    string `xml:"version"`
}

// id returns a maven locator from dependency information.
func (d Dependency) id() string {
	return d.GroupID + ":" + d.ArtifactID
}

// Shell implements Output and uses Cmd to generate the DependencyGraph.
type Shell struct {
	Cmd func(...string) (string, *errors.Error)
}

// ShellOutput creates a Shell which shells out to the supplied binary to generate values to return.
func ShellOutput(binary, dir string) Output {
	return Shell{
		Cmd: func(args ...string) (string, *errors.Error) {
			cmd := exec.Cmd{
				Name: binary,
				Argv: args,
				Dir:  dir,
			}

			stdout, stderr, err := exec.Run(cmd)
			if stderr != "" || err != nil {
				fmt.Println("error")
				return stdout, &errors.Error{
					Cause:           err,
					Type:            errors.Exec,
					Troubleshooting: fmt.Sprintf("Ensure that %s is installed correctly, if it is then try to run %s %s.\nstdout: %s\nstderr:%s", binary, binary, args, stdout, stderr),
				}
			}

			return stdout, nil
		},
	}
}

// ValidBinary attempts to run a list of `lein` commands and returns the most preferred.
func ValidBinary(dir string) (string, error) {
	lein, _, err := exec.Which("--version", os.Getenv("FOSSA_LEIN_CMD"), "lein")
	return lein, err
}

//go:generate bash -c "genny -in=$GOPATH/src/github.com/fossas/fossa-cli/graph/readtree.go gen 'Generic=Dependency' | sed -e 's/package graph/package leiningen/' > readtree_generated.go"

// DependencyGraph uses a Shell's Cmd to generate leiningen formatted output which is
// massaged into a dependency graph.
func (s Shell) DependencyGraph(target string) (graph.Deps, *errors.Error) {
	out, err := s.Cmd("deps", ":tree")
	if err != nil {
		return graph.Deps{}, err
	}

	var filteredLines []string
	splitReg := regexp.MustCompile("\r?\n")
	for _, line := range splitReg.Split(out, -1) {
		if strings.HasPrefix(line, "Retrieving") || line == "" {
			continue
		}
		filteredLines = append(filteredLines, line)
	}

	spaceFinder := regexp.MustCompile(` *`)
	imports, deps, readErr := ReadDependencyTree(filteredLines, func(line string) (int, Dependency, error) {
		log.WithField("line", line).Debug("parsing output line")
		var depth int

		// Count leading spaces.
		spaces := spaceFinder.FindStringSubmatch(line)
		if len(spaces) != 0 {
			depth = (len(spaces[0]) / 2) + 1
		}

		return depth, dependencyFromLine(line), nil
	})
	if readErr != nil {
		return graph.Deps{}, nil
	}

	return graphFromDependencies(imports, deps), nil
}

func dependencyFromLine(line string) Dependency {
	var groupID, artifactID, revision string
	nameFinder := regexp.MustCompile(`\[[^ ]* `)
	revisionFinder := regexp.MustCompile(`".*"`)

	// Find dependency name after leading bracket and trim bracket and space.
	name := nameFinder.FindStringSubmatch(line)
	if len(name) > 0 {
		nameClean := strings.TrimSpace(strings.ReplaceAll(name[0], "[", ""))
		depSplit := strings.Split(nameClean, "/")
		if len(depSplit) != 0 {
			groupID = depSplit[0]
			artifactID = depSplit[0]
			if len(depSplit) > 1 {
				artifactID = depSplit[1]
			}
		}
	}

	// Find revision in quotations and trim quotations.
	revisionSplit := revisionFinder.FindAllString(line, 1)
	if len(revisionSplit) != 0 {
		revision = strings.ReplaceAll(revisionSplit[0], `"`, "")
	}

	return Dependency{GroupID: groupID, ArtifactID: artifactID, Version: revision}
}

// ProjectFileDependencies returns the dependencies listed in a clojure project file.
func ProjectFileDependencies(dir, file string) (graph.Deps, error) {
	depGraph := graph.Deps{Transitive: make(map[pkg.ID]pkg.Package)}
	dependenciesBlock := false
	brackets := 0

	projectFile := filepath.Join(dir, file)
	project, err := files.Read(projectFile)
	if err != nil {
		return graph.Deps{}, &errors.Error{
			Cause:           err,
			Type:            errors.Unknown,
			Troubleshooting: fmt.Sprintf("Clojure project file: `%s` could not be read. Ensure that it exists or remove the module if you do not believe it should be analyzed.", projectFile),
		}
	}

	for _, line := range strings.Split(string(project), "\n") {
		// 1. Check for empty line and comments.
		// 2. Check for start of dependencies block.
		// 3. Check for end of the dependencies block.
		trimLine := strings.TrimSpace(line)
		if trimLine == "" || strings.Contains(trimLine, ";") {
			continue
		} else if strings.Contains(trimLine, ":dependencies") {
			dependenciesBlock = true
			trimLine = strings.ReplaceAll(trimLine, ":dependencies", "")
		} else if strings.Contains(trimLine, ":") && !strings.Contains(trimLine, ":exclusions") {
			dependenciesBlock = false
			continue
		}

		// Check for a continuing dependencies block. Test dep four for reference.
		if dependenciesBlock && brackets < 2 {
			dep := dependencyFromLine(trimLine)
			pkgID := pkg.ID{
				Type:     pkg.Maven,
				Name:     dep.id(),
				Revision: dep.Version,
			}

			depGraph.Direct = append(depGraph.Direct, pkg.Import{
				Target:   dep.id(),
				Resolved: pkgID,
			})
			depGraph.Transitive[pkgID] = pkg.Package{ID: pkgID}
		}
		brackets = brackets + strings.Count(line, "[") - strings.Count(line, "]")
	}

	return depGraph, nil
}

func graphFromDependencies(direct []Dependency, transitive map[Dependency][]Dependency) graph.Deps {
	imports := []pkg.Import{}
	for _, dep := range direct {
		imports = append(imports, pkg.Import{
			Resolved: pkg.ID{
				Type:     pkg.Maven,
				Name:     dep.id(),
				Revision: dep.Version,
			},
		})
	}

	pkgGraph := make(map[pkg.ID]pkg.Package)
	for parent, children := range transitive {
		dep := pkg.Package{
			ID: pkg.ID{
				Type:     pkg.Maven,
				Name:     parent.id(),
				Revision: parent.Version,
			},
			Imports: make([]pkg.Import, 0, len(children)),
		}
		for _, child := range children {
			dep.Imports = append(dep.Imports, pkg.Import{
				Target: child.Version,
				Resolved: pkg.ID{
					Type:     pkg.Maven,
					Name:     child.id(),
					Revision: child.Version,
				},
			})
		}
		pkgGraph[dep.ID] = dep
	}

	return graph.Deps{Direct: imports, Transitive: pkgGraph}
}

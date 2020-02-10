package bazel

import (
	"encoding/xml"
	"fmt"
	"strings"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

// Shell contains a command which returns bazel query output.
type Shell struct {
	Cmd func(...string) (string, *errors.Error)
}

// ExecutableShell creates a Shell which shells out to the supplied binary to generate values to return.
func ExecutableShell(binary, dir string) Shell {
	return Shell{
		Cmd: func(args ...string) (string, *errors.Error) {
			cmd := exec.Cmd{
				Name: binary,
				Argv: args,
				Dir:  dir,
			}

			stdout, stderr, err := exec.Run(cmd)
			if err != nil {
				return stdout, &errors.Error{
					Cause:           err,
					Type:            errors.Exec,
					Troubleshooting: fmt.Sprintf("Fossa could not run `%s %s` within the current directory. Try running this command and ensure that bazel is installed in your environment.\nstdout: %s\nstderr: %s", binary, strings.Join(args, " "), stdout, stderr),
				}
			}

			return stdout, nil
		},
	}
}

// TargetDependencies returns a dependency graph corresponding to a bazel target.
func (s Shell) TargetDependencies(target string, upload bool) (graph.Deps, error) {
	queryString := "deps(" + target + ")"
	output, err := s.Cmd("query", queryString, "--output", "xml")
	if err != nil {
		return graph.Deps{}, err
	}

	packages, err := packagesFromOutput(output, target, upload)
	if err != nil {
		return graph.Deps{}, err
	}

	return graphFromPackages(packages), nil
}

// Query is the xml result of a Bazel query.
type Query struct {
	Rules []Rule `xml:"rule"`
}

// Rule maps to the xml representation of a Bazel rule https://docs.bazel.build/versions/2.0.0/skylark/rules.html.
type Rule struct {
	Class    string   `xml:"class,attr"`
	Name     string   `xml:"name,attr"`
	Location string   `xml:"location,attr"`
	Values   []Values `xml:"string"`
}

// Values is the list of string values present in a rule. They function as a key value map.
type Values struct {
	Key   string `xml:"name,attr"`
	Value string `xml:"value,attr"`
}

func packagesFromOutput(commandOutput string, target string, upload bool) ([]pkg.ID, *errors.Error) {
	lines := strings.Split(commandOutput, "\n")
	if strings.Contains(lines[0], `version="1.1"`) {
		lines[0] = `<?xml version="1.0" encoding="UTF-8"?>`
	}

	var query Query
	err := xml.Unmarshal([]byte(strings.Join(lines, "\n")), &query)
	if err != nil {
		return nil, &errors.Error{
			Cause:           err,
			Troubleshooting: fmt.Sprintf("FOSSA was unable to read the xml output from the Bazel command. This may be related to conflicting versions of xml as version 1.1 is unsupported by the FOSSA CLI. If you run into this issue please send the results of the command `bazel query 'deps(%s)' --output xml` to fossa.", target),
		}
	}

	cLibraries := map[string]bool{}
	packages := []pkg.ID{}
	for _, rule := range query.Rules {
		pack := pkg.ID{}
		switch rule.Class {
		case "go_library":
			fallthrough
		case "dbx_go_library":
			if strings.Contains(rule.Name, "dropbox") {
				continue
			}
			pack.Type = pkg.Go
			pack.Name = goNameFromRule(rule.Name, target)
		case "_npm_library_internal":
			fallthrough
		case "dbx_npm_library":
			pack.Type = pkg.NodeJS
			requirement := valueFromKey(rule.Values, "npm_req")
			pack.Name, pack.Revision = npmReqFromString(requirement)
		case "py_library": //py_library dependencies appear to all be unknown or custom dependencies.
			continue
		case "dbx_py_pypi_piplib_internal":
			fallthrough
		case "dbx_py_pypi_piplib":
			pack.Type = pkg.Python
			pack.Name = valueFromKey(rule.Values, "name")
			pack.Revision = valueFromKey(rule.Values, "pip_version")
		case "rust_library":
			pack.Type = pkg.Rust
			pack.Name = valueFromKey(rule.Values, "name")
			pack.Revision = valueFromKey(rule.Values, "version")
		case "cc_library":
			fallthrough
		case "dbx_thirdparty_cc_library":
			splitLocation := strings.Split(rule.Location, ":")
			if len(splitLocation) < 1 {
				log.Debugf("cc library %s was unable to be uploaded because FOSSA was unable to find a directory for the library. rule: %+v", rule.Name, rule)
				continue
			}

			trimmed := strings.TrimSuffix(strings.TrimSuffix(splitLocation[0], "BUILD"), "BUILD.bazel")
			if _, ok := cLibraries[trimmed]; ok || strings.Contains(trimmed, ".cache") {
				continue
			}
			cLibraries[trimmed] = true

			loc, err := fossa.UploadTarballDependency(trimmed, upload, true)
			if err != nil {
				log.Debugf("cc library %s was unable to be uploaded. rule: %+v. error: %s", rule.Name, rule, err.Error())
				continue
			}

			pack.Type = pkg.Raw
			pack.Name = loc.Project
			pack.Revision = loc.Revision
		default:
			continue
		}

		packages = append(packages, pack)
	}

	return packages, nil
}

func goNameFromRule(ruleName string, target string) string {
	trimmedTarget := strings.TrimSuffix(target, "...")
	trimmedRuleName := strings.TrimPrefix(ruleName, "//go/src/")
	trimmedRuleName = strings.TrimPrefix(trimmedRuleName, trimmedTarget)
	trimmedRuleName = strings.TrimPrefix(trimmedRuleName, "//"+trimmedTarget)
	nameSplit := strings.Split(trimmedRuleName, ":")
	if len(nameSplit) > 0 {
		return nameSplit[0]
	}

	return ""
}

// Handle the npm_req field formats. Name and version are separated by "@" and "@" can be present in the name.
func npmReqFromString(req string) (string, string) {
	prefixAt := false
	requirement := req
	if requirement[0] == '@' {
		requirement = strings.TrimPrefix(requirement, "@")
		prefixAt = true
	}

	name, version := "", ""
	npmSpec := strings.Split(requirement, "@")
	if len(npmSpec) > 1 {
		version = npmSpec[1]
	}
	if len(npmSpec) > 0 {
		name = npmSpec[0]
	}
	if prefixAt {
		name = "@" + name
	}

	return name, version
}

func valueFromKey(values []Values, key string) string {
	for _, value := range values {
		if value.Key == key {
			return value.Value
		}
	}
	return ""
}

func graphFromPackages(packages []pkg.ID) graph.Deps {
	depGraph := graph.Deps{Transitive: map[pkg.ID]pkg.Package{}}
	for _, p := range packages {
		depGraph.Direct = append(depGraph.Direct, pkg.Import{Target: p.Name, Resolved: p})
		depGraph.Transitive[p] = pkg.Package{ID: p}
	}

	return depGraph
}

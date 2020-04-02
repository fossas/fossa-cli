package yarn

import (
	"encoding/json"
	"os"
	"strings"

	"github.com/Masterminds/semver"
	"github.com/apex/log"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type YarnTool interface {
	// TODO: implement yarn list --production --frozen-lockfile. Skipping for now because the implementation relies 100% on yarn.lock
	// List(dir string) error
	Install(dir string) error
	// Exists returns true if yarn is available
	Exists() bool
	// List returns a dependency graph.
	List(target string, devDependencies bool) (graph.Deps, error)
}

type SystemYarn struct {
	Cmd string
}

func New() (YarnTool, error) {
	yarnCmd, _, yarnErr := exec.Which("-v", os.Getenv("FOSSA_NPM_CMD"), "yarn")
	if yarnErr != nil {
		log.Debugf("Could not find Yarn %s", yarnErr.Error())
		return SystemYarn{}, yarnErr
	}

	return SystemYarn{
		Cmd: yarnCmd,
	}, nil
}

func (y SystemYarn) Exists() bool {
	return y.Cmd != ""
}

func (y SystemYarn) Install(dir string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: y.Cmd,
		Argv: []string{"install", "--production", "--frozen-lockfile"},
		Dir:  dir,
	})

	return err
}

type Output struct {
	Data Data
}
type Data struct {
	Trees []Child
}
type Child struct {
	Name     string
	Color    string
	Children []Child
}

func (y SystemYarn) List(dir string, devDependencies bool) (graph.Deps, error) {
	listCmd := exec.Cmd{
		Name:    y.Cmd,
		Argv:    []string{"list", "--json"},
		Dir:     dir,
		WithEnv: map[string]string{"NODE_ENV": "production"},
	}
	if !devDependencies {
		listCmd.WithEnv = map[string]string{"NODE_ENV": "production"}
	}

	stdout, _, err := exec.Run(listCmd)
	if err != nil {
		return graph.Deps{}, err
	}

	var output Output
	err = json.Unmarshal([]byte(stdout), &output)
	if err != nil {
		return graph.Deps{}, err
	}

	return listToGraph(output)
}

// list to graph turns yarn list output into a dependency graph.
// First we look through the graph and collect all pinned versions.
// Next we resolve dependency specs to create a full graph.
func listToGraph(output Output) (graph.Deps, error) {
	direct := pkg.Imports{}
	dependencies := make(map[string]map[string]pkg.ID)
	for _, child := range output.Data.Trees {
		name, version := npmReqFromString(child.Name)
		id := pkg.ID{
			Type:     pkg.NodeJS,
			Name:     name,
			Revision: version,
		}

		if dependencies[name] == nil {
			dependencies[name] = make(map[string]pkg.ID)
		}

		dependencies[name][version] = id
		direct = append(direct, pkg.Import{
			Target:   name,
			Resolved: id,
		})

		recurseChildren(child, dependencies)
	}

	transitive := make(map[pkg.ID]pkg.Package)
	for _, child := range output.Data.Trees {
		resolveDependencyGraph(child, dependencies, transitive)
	}

	return graph.Deps{
		Direct:     direct,
		Transitive: transitive,
	}, nil
}

func resolveDependencyGraph(child Child, deps map[string]map[string]pkg.ID, transitive map[pkg.ID]pkg.Package) {
	imports := []pkg.Import{}
	for _, nestedChild := range child.Children {
		name, spec := npmReqFromString(nestedChild.Name)

		for version, id := range deps[name] {
			if versionWithinSpec(version, spec) {
				imports = append(imports, pkg.Import{
					Target:   name,
					Resolved: id,
				})
				break
			}
		}

		resolveDependencyGraph(nestedChild, deps, transitive)
	}

	name, version := npmReqFromString(child.Name)
	childID, ok := deps[name][version]
	if ok {
		transitive[childID] = pkg.Package{
			ID:      childID,
			Imports: imports,
		}
	}
}

func versionWithinSpec(version, spec string) bool {
	c, err := semver.NewConstraint(spec)
	if err != nil {
		return false
	}
	v, err := semver.NewVersion(version)
	if err != nil {
		return false
	}
	return c.Check(v)
}

// recurseChildren traverses each child's dependency graph and adds resolved dependencies
// to the dependency map
func recurseChildren(child Child, dependencies map[string]map[string]pkg.ID) {
	for _, nestedChild := range child.Children {
		if nestedChild.Color == "" || nestedChild.Color == "bold" {
			name, version := npmReqFromString(nestedChild.Name)
			if dependencies[name] == nil {
				dependencies[name] = make(map[string]pkg.ID)
			}

			dependencies[name][version] = pkg.ID{
				Type:     pkg.NodeJS,
				Name:     name,
				Revision: version,
			}
		}

		recurseChildren(nestedChild, dependencies)
	}
}

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

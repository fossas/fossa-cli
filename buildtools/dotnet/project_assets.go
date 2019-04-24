package dotnet

import (
	"strings"

	"github.com/fossas/fossa-cli/files"
)

type Lockfile struct {
	Version int
	Targets map[string]map[string]Target

	resolved map[string]resolved
}

type Target struct {
	Type         string
	Dependencies map[string]string
}

type resolved struct {
	Version string
	Imports map[string]string
}

func (l *Lockfile) resolve(pkg string) string {
	return l.resolved[pkg].Version
}

func (l *Lockfile) imports(pkg string) map[string]string {
	return l.resolved[pkg].Imports
}

func readLockfile(filename string) (Lockfile, error) {
	var lockfile Lockfile
	err := files.ReadJSON(&lockfile, filename)
	if err != nil {
		return Lockfile{}, err
	}

	lockfile.resolved = make(map[string]resolved)
	for _, deps := range lockfile.Targets {
		for key, dep := range deps {
			sections := strings.Split(key, "/")
			name := sections[0]
			version := sections[1]
			lockfile.resolved[name] = resolved{
				Version: version,
				Imports: dep.Dependencies,
			}
		}
	}

	return lockfile, nil
}

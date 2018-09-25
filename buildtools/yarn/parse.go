package yarn

import (
	"path/filepath"
	"regexp"

	"github.com/fossas/fossa-cli/buildtools/npm"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"gopkg.in/yaml.v2"
)

type lockfileEntry struct {
	Version      string
	Dependencies map[string]string
}

type YarnLockfile map[string]lockfileEntry

// FromLockfile builds a dependency graph based on the provided path to yarn.lock
func FromLockfile(pathElems ...string) (graph.Deps, error) {
	filePath := filepath.Join(pathElems...)
	yarnLockfileExists, err := files.ExistsFolder(filePath)
	if err != nil {
		return graph.Deps{}, err
	}
	npm.FromManifest()
	lockfile, err := readLockfile(filePath)
	if err != nil {
		return graph.Deps{}, nil
	}

	return graph.Deps{}, nil
}

func readLockfile(pathElems ...string) (YarnLockfile, error) {
	var lockfile YarnLockfile

	filePath := filepath.Join(pathElems...)

	fileContent, err := files.Read(filePath)
	if err != nil {
		return YarnLockfile{}, err
	}

	r, err := regexp.Compile("\\s\"")
	yamlCompatLockfile := r.ReplaceAll(fileContent, []byte(": \""))
	println(string(fileContent))

	err = yaml.Unmarshal(yamlCompatLockfile, &lockfile)
	if err != nil {
		return YarnLockfile{}, err
	}

	return lockfile, nil
}

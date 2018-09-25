package yarn

import (
	"path/filepath"
	"regexp"

	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"gopkg.in/yaml.v2"
)

type lockfileEntry struct {
	Version      string
	Dependencies map[string]string
}

type YarnLockfile map[string]lockfileEntry

func FromLockfile(pathElems ...string) (graph.Deps, error) {
	// filePath := filepath.Join(pathElems...)
	// lockfile, err := readLockfile(filePath)

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

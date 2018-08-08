// Package files implements utility routines for finding and reading files.
package files

import (
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/apex/log"
)

func fileMode(elem ...string) (os.FileMode, error) {
	file, err := os.Stat(filepath.Join(elem...))
	if err != nil {
		return 0, err
	}

	return file.Mode(), nil
}

func Exists(pathElems ...string) (bool, error) {
	mode, err := fileMode(pathElems...)
	if notExistErr(err) {
		return false, nil
	}
	if err != nil {
		return false, err
	}
	return mode.IsRegular(), nil
}

func ExistsFolder(pathElems ...string) (bool, error) {
	mode, err := fileMode(pathElems...)
	if notExistErr(err) {
		return false, nil
	}
	if err != nil {
		return false, err
	}
	return mode.IsDir(), nil
}

func Read(pathElems ...string) ([]byte, error) {
	name := filepath.Join(pathElems...)

	log.WithField("filename", name).Debug("reading file")
	contents, err := ioutil.ReadFile(name)
	if err != nil {
		log.WithError(err).WithField("filename", name).Debug("unable to read file")
	}

	return contents, err
}

// os.IsNotExist doesn't handle non-existent parent directories e.g.
// stat /some/path/without/a/parent.json: not a directory
func notExistErr(err error) bool {
	if os.IsNotExist(err) {
		return true
	}
	if _, ok := err.(*os.PathError); ok {
		return true
	}
	return false
}

func Rm(pathElems ...string) error {
	return os.RemoveAll(filepath.Join(pathElems...))
}

package files

import (
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/fossas/fossa-cli/log"
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
	if os.IsNotExist(err) {
		return false, nil
	}
	if err != nil {
		return false, err
	}
	return mode.IsRegular(), nil
}

func ExistsFolder(pathElems ...string) (bool, error) {
	mode, err := fileMode(pathElems...)
	if os.IsNotExist(err) {
		return false, nil
	}
	if err != nil {
		return false, err
	}
	return mode.IsDir(), nil
}

func ReadFile(pathElems ...string) ([]byte, error) {
	name := filepath.Join(pathElems...)

	log.Logger.Debugf("Reading file `%s`", name)
	contents, err := ioutil.ReadFile(name)
	if err != nil {
		log.Logger.Debugf("Could not read file `%s`: %s", name, err.Error())
	}

	return contents, err
}

package common

import (
	"encoding/json"
	"io/ioutil"
	"os"
	"path/filepath"
)

type fileImpl struct {
	logger LogService
}

func newFileService(logger LogService) FileService {
	return &fileImpl{logger: logger}
}

func fileMode(elem ...string) (os.FileMode, error) {
	file, err := os.Stat(filepath.Join(elem...))
	if err != nil {
		return 0, err
	}

	return file.Mode(), nil
}

func (fs *fileImpl) HasFile(pathElems ...string) bool {
	mode, err := fileMode(pathElems...)
	if err != nil {
		return false
	}

	return mode.IsRegular()
}

func (fs *fileImpl) HasFolder(pathElems ...string) bool {
	mode, err := fileMode(pathElems...)
	if err != nil {
		return false
	}

	return mode.IsDir()
}

func (fs *fileImpl) ReadFile(pathElems ...string) ([]byte, error) {
	name := filepath.Join(pathElems...)
	fs.logger.Debugf("Reading file `%s`", name)
	contents, err := ioutil.ReadFile(name)
	if err != nil {
		fs.logger.Debugf("Could not read file `%s`: %s", name, err.Error())
	}
	return contents, err
}

func (fs *fileImpl) ReadJSON(v interface{}, pathElems ...string) error {
	return fs.ReadUnmarshal(json.Unmarshal, v, pathElems...)
}

func (fs *fileImpl) ReadUnmarshal(unmarshal UnmarshalFunc, v interface{}, pathElems ...string) error {
	name := filepath.Join(pathElems...)
	fs.logger.Debugf("Parsing file `%s`", name)
	contents, err := fs.ReadFile(pathElems...)
	if err != nil {
		return err
	}
	err = unmarshal(contents, v)
	if err != nil {
		fs.logger.Debugf("Could not parse file `%s`: %s", name, err.Error())
	}
	return err
}

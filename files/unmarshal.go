package files

import (
	"encoding/json"

	"github.com/BurntSushi/toml"

	"github.com/fossas/fossa-cli/log"
)

func ReadJSON(v interface{}, path string) error {
	return ReadUnmarshal(v, path, json.Unmarshal)
}

func ReadTOML(v interface{}, path string) error {
	return ReadUnmarshal(v, path, toml.Unmarshal)
}

type UnmarshalFunc func(data []byte, v interface{}) error

func ReadUnmarshal(v interface{}, path string, unmarshal UnmarshalFunc) error {
	log.Logger.Debugf("Parsing file `%s`", path)
	contents, err := ReadFile(path)
	if err != nil {
		return err
	}
	err = unmarshal(contents, v)
	if err != nil {
		log.Logger.Debugf("Could not parse file `%s`: %s", path, err.Error())
	}
	return err
}

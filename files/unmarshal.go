package files

import (
	"encoding/json"
	"encoding/xml"
	"path/filepath"

	"github.com/BurntSushi/toml"
	yaml "gopkg.in/yaml.v2"

	"github.com/apex/log"
)

// ReadJSON reads and unmarshals a file as if it contained JSON.
func ReadJSON(v interface{}, pathElems ...string) error {
	return ReadUnmarshal(json.Unmarshal, v, pathElems...)
}

// ReadTOML reads and unmarshals a file as if it contained TOML.
func ReadTOML(v interface{}, pathElems ...string) error {
	return ReadUnmarshal(toml.Unmarshal, v, pathElems...)
}

// ReadXML reads and unmarshals a file as if it contained XML.
func ReadXML(v interface{}, pathElems ...string) error {
	return ReadUnmarshal(xml.Unmarshal, v, pathElems...)
}

// ReadYAML reads and unmarshals a file as if it contained YAML.
func ReadYAML(v interface{}, pathElems ...string) error {
	return ReadUnmarshal(yaml.Unmarshal, v, pathElems...)
}

// An UnmarshalFunc is a function for unmarshalling bytes into values.
type UnmarshalFunc func(data []byte, v interface{}) error

// ReadUnmarshal reads a file and then unmarshals its contents using an
// UnmarshalFunc.
func ReadUnmarshal(unmarshal UnmarshalFunc, v interface{}, pathElems ...string) error {
	filename := filepath.Join(pathElems...)
	log.WithField("filename", filename).Debug("unmarshalling file")
	contents, err := Read(pathElems...)
	if err != nil {
		return err
	}
	err = unmarshal(contents, v)
	if err != nil {
		log.
			WithError(err).
			WithField("filename", filename).
			Debug("unable to unmarshal file")
	}
	return err
}

package bower

import (
	"fmt"
	"path/filepath"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
)

// Config models a bower configuration file: https://bower.io/docs/config/.
type Config struct {
	CWD       string `json:"cwd"`
	Directory string `json:"directory"`
}

// TODO: this is nowhere near complete -- there's all sorts of crazy cascading
// mechanisms that configure Bower.
func ReadConfig(dir string) (Config, *errors.Error) {
	ok, err := files.Exists(dir, ".bowerrc")
	if ok {
		return ReadConfigFile(filepath.Join(dir, ".bowerrc"))
	}
	return Config{
			CWD:       dir,
			Directory: filepath.Join(dir, "bower_components"),
		}, &errors.Error{
			Cause:           err,
			Type:            errors.Unknown,
			Troubleshooting: fmt.Sprintf("The file %s.bowerrc cannot be verified to exist. Ensure that it is present.", dir),
		}
}

// ReadConfigFile attempts to unmarshal the Bower configuration file.
func ReadConfigFile(filename string) (Config, *errors.Error) {
	var config Config
	err := files.ReadJSON(&config, filename)
	if err != nil {
		return Config{}, &errors.Error{
			Cause:           err,
			Type:            errors.Unknown,
			Troubleshooting: fmt.Sprintf("The bower configuration `%s` file was unable to be read. Check to see if the file exists, can be opened, and is properly formatted JSON", filename),
			Link:            "https://bower.io/docs/config/",
		}
	}
	if config.Directory == "" {
		config.Directory = "bower_components"
	}
	return config, nil
}

package bower

import (
	"path/filepath"

	"github.com/fossas/fossa-cli/files"
)

type Config struct {
	CWD       string `json:"cwd"`
	Directory string `json:"directory"`
}

// TODO: this is nowhere near complete -- there's all sorts of crazy cascading
// mechanisms that configure Bower.
func ReadConfig(dir string) (Config, error) {
	ok, err := files.Exists(dir, ".bowerrc")
	if err != nil {
		return Config{}, err
	}
	if ok {
		return ReadConfigFile(filepath.Join(dir, ".bowerrc"))
	}
	return Config{
		CWD:       dir,
		Directory: filepath.Join(dir, "bower_components"),
	}, nil
}

func ReadConfigFile(filename string) (Config, error) {
	var config Config
	err := files.ReadJSON(&config, filename)
	if err != nil {
		return Config{}, err
	}
	return config, nil
}

package bower

import (
	"path/filepath"

	"github.com/fossas/fossa-cli/files"
)

type Config struct {
	CWD       string `json:"cwd"`
	Directory string `json:"directory"`
}

func NewConfig(cwd string) Config {
	return Config{
		CWD:       cwd,
		Directory: filepath.Join(cwd, "bower_components"),
	}
}

// TODO: this is nowhere near complete -- there's all sorts of crazy cascading
// mechanisms that configure Bower.
func ReadConfig(dir string) (Config, error) {
	config := NewConfig(dir)
	ok, err := files.Exists(dir, ".bowerrc")
	if ok {
		return ReadConfigFile(filepath.Join(dir, ".bowerrc"))
	}
	return config, err
}

func ReadConfigFile(filename string) (Config, error) {
	config := NewConfig(filepath.Dir(filename))
	err := files.ReadJSON(&config, filename)
	if err != nil {
		return config, err
	}
	return config, nil
}

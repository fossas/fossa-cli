package bower

import "github.com/fossas/fossa-cli/errutil"

type Config struct {
	CWD       string `json:"cwd"`
	Directory string `json:"directory"`
}

func ReadConfig(dir string) (Config, error) {
	return Config{}, errutil.ErrNotImplemented
}

func ReadConfigFile(filename string) (Config, error) {
	return Config{}, errutil.ErrNotImplemented
}

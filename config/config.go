// Package config implements application-level configuration functionality.
//
// It works by loading configuration sources (e.g. CLI flags, configuration
// files, local VCS) and providing functions which compute relevant
// configuration values from these sources.
//
// This design is intended to make how a particular value is computed very
// clear. All values can have their computation strategy modified independently
// of all other values. It should also be easy to determine which source set a
// particular configuration value.
package config

import (
	"github.com/apex/log"
	"github.com/urfave/cli"
	git "gopkg.in/src-d/go-git.v4"

	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	v1 "github.com/fossas/fossa-cli/config/file.v1"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/vcs"
)

var (
	ctx  *cli.Context
	repo *git.Repository
	file File = NoFile{}
)

// Init initializes application-level configuration.
func Init(c *cli.Context) error {
	// First, set the CLI flags.
	ctx = c

	// Second, try to load a configuration file.
	f, fname, err := ReadFile(c)
	log.Logger.Debugf("Loaded configuration file: %#v %#v", f, fname)
	if err != nil {
		return err
	}
	file = f
	filename = fname

	// Third, try to open the local VCS repository.
	vcsDir, err := vcs.Nearest(".")
	if err == vcs.ErrNoNearestVCS {
		return nil
	}
	if err != nil {
		return err
	}
	r, err := git.PlainOpen(vcsDir)
	if err == git.ErrRepositoryNotExists {
		return nil
	}
	if err != nil {
		return err
	}
	repo = r
	return nil
}

// ReadFile reads the configuration file specified by CLI flags.
func ReadFile(c *cli.Context) (File, string, error) {
	// Find a configuration file if one exists.
	flag := ctx.String(flags.Config)
	log.Logger.Debugf("Trying to find configuration file at %#v", flag)
	filename, err := TryFiles(flag, ".fossa.yml", ".fossa.yaml")
	if err == ErrFileNotFound {
		return NoFile{}, ".fossa.yml", nil
	}
	if err != nil {
		return NoFile{}, ".fossa.yml", err
	}

	// Try to unmarshal the configuration file into a known config file version.
	data, err := files.Read(filename)
	if err != nil {
		return NoFile{}, filename, err
	}
	file, err := v1.New(data)
	if err != nil {
		return NoFile{}, filename, err
	}
	return file, filename, err
}

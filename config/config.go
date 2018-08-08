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
	if err != nil {
		return err
	}

	log.WithField("filename", fname).Debug("loaded configuration file")
	file = f
	filename = fname

	// Third, try to open the local VCS repository.
	_, dir, err := vcs.Nearest(".")
	if err == vcs.ErrNoNearestVCS {
		return nil
	}
	if err != nil {
		return err
	}
	r, err := git.PlainOpen(dir)
	if err == git.ErrRepositoryNotExists {
		return nil
	}
	if err != nil {
		return err
	}
	repo = r

	return nil
}

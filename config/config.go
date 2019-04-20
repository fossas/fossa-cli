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
	"github.com/fossas/fossa-cli/vcs"
	"github.com/urfave/cli"
)

var (
	ctx  *cli.Context
	file File = NoFile{}

	repo vcs.System
)

// SetContext initializes application-level configuration.
func SetContext(c *cli.Context) error {
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
	vcsType, dir, err := vcs.Nearest(".")
	switch err {
	case nil:
		// OK
	case vcs.ErrNoNearestVCS:
		vcsType = vcs.None
		dir = "."
	default:
		return err
	}

	var r vcs.System

	switch vcsType {
	case vcs.Git:
		r, err = vcs.NewGitRepository(dir)
	case vcs.Subversion:
		r, err = vcs.NewSubversionRepository(dir)
	case vcs.None:
		r, err = vcs.NewNoRepository(dir)
	}

	if err != nil {
		return err
	}

	repo = r
	return nil
}

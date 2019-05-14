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

	"github.com/fossas/fossa-cli/vcs"
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
	if err != nil {
		if err == vcs.ErrNoNearestVCS {
			vcsType = vcs.None
			dir = "."
		} else {
			return err
		}
	}

	var r vcs.System

	switch vcsType {
	case vcs.Git:
		r, err = vcs.NewGitRepository(dir)
	case vcs.Subversion:
		r, err = vcs.NewSubversionRepository(dir)
	case vcs.Mercurial:
		r, err = vcs.NewMercurialRepository(dir)
	case vcs.None:
		r, err = vcs.NewNoRepository(dir)
	default:
		log.Warnf("FOSSA is unable to extract VCS context from your %s project, which means you will not "+
			"be able to take advantage of VCS history information on fossa.com, but a scan will run.", vcsType)
		r, err = vcs.NewNoRepository(dir)
	}

	if err != nil {
		return err
	}

	repo = r
	return nil
}

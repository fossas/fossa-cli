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
	"gopkg.in/src-d/go-git.v4"

	"github.com/fossas/fossa-cli/vcs"
)

var (
	ctx  *cli.Context
	file File = NoFile{}

	repoOLD *git.Repository

	Repo Repository
)

// A Repository represents the current state of a version-controlled repository.
type Repository interface {
	Project() string
	Head() Revision
}

type Revision struct {
	Branch     string
	RevisionID string
}

type GitRepository struct {
}

func (gr *GitRepository) Head() Revision {
	panic("implement me")
}

func (gr *GitRepository) Project() string {
	origin, err := repoOLD.Remote("origin")
	if err == nil && origin != nil {
		return origin.Config().URLs[0]
	}
	return ""
}

type SubversionRepository struct {
}

func (*SubversionRepository) Project() string {
	panic("implement me")
}

func (*SubversionRepository) Head() Revision {
	panic("implement me")
}

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
	case vcs.ErrNoNearestVCS:
		return nil
	case vcs.ErrUnsupportedVCS:
		// Construct a revision ID.
	default:
		return err
	}

	switch vcsType {
	case vcs.Git:
		r, err := git.PlainOpen(dir)
		if err == git.ErrRepositoryNotExists {
			return nil
		}
		if err != nil {
			return err
		}
		repoOLD = r
	case vcs.Subversion:
	}

	return nil
}

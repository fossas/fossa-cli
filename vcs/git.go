package vcs

import (
	"strings"

	"gopkg.in/src-d/go-git.v4"

	"github.com/fossas/fossa-cli/errors"
)

// GitRepository implements the System interface.
type GitRepository struct {
	r    *git.Repository
	dir  string
	head Revision
}

// NewGitRepository uses the git repository's metadata at dir to identify the codebase.
func NewGitRepository(dir string) (*GitRepository, error) {
	r, err := git.PlainOpen(dir)
	if err != nil {
		return nil, err
	}

	ref, err := r.Head()
	if err != nil {
		return nil, errors.Wrap(err, "could not get HEAD of git repository")
	}
	return &GitRepository{
		r:   r,
		dir: dir,
		head: Revision{
			Branch:     strings.TrimPrefix(ref.Name().String(), "refs/heads/"),
			RevisionID: ref.Hash().String(),
		},
	}, nil
}

func (gr *GitRepository) Head() Revision { return gr.head }

func (gr *GitRepository) Project() string {
	origin, err := gr.r.Remote("origin")
	if err == nil && origin != nil {
		return origin.Config().URLs[0]
	}
	return ""
}

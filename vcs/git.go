package vcs

import (
	"gopkg.in/src-d/go-git.v4"
)

type GitRepository struct {
	r   *git.Repository
	dir string
}

// NewGitRepository takes the directory where a git repository exists and returns an implementation of the
// System interface that uses the repository's metadata.
func NewGitRepository(dir string) (*GitRepository, error) {
	r, err := git.PlainOpen(dir)
	if err == git.ErrRepositoryNotExists {
		return nil, err
	}
	return &GitRepository{
		r:   r,
		dir: dir,
	}, nil
}

func (gr *GitRepository) Head() Revision {
	ref, err := gr.r.Head()
	if err != nil {
		panic(err)
	}
	return Revision{
		Branch:     ref.Name().String(),
		RevisionID: ref.Hash().String(),
	}
}

func (gr *GitRepository) Project() string {
	origin, err := gr.r.Remote("origin")
	if err == nil && origin != nil {
		return origin.Config().URLs[0]
	}
	return ""
}

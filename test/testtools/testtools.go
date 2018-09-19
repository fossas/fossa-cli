package testtools
 import (
	git "gopkg.in/src-d/go-git.v4"
	"gopkg.in/src-d/go-git.v4/plumbing"
)
 type Initializer interface {
	// FixtureDirectory returns the directory of the test project fixtures
	FixtureDirectory() string
 	// BuildAll fetches source code and runs all install/build scrips necessary for analysis
	BuildAll() error
 	// RemoveAll removes all artifacts from BuildAll
	RemoveAll() error
}
 // SetupTests executes the initializer's build all and returns a function to be deferred for teardown. SetupTests and it's returned function are meant to be called once per analyzer type
func SetupTests(initializer Initializer) (func(), error) {
	err := initializer.BuildAll()
 	// TODO: ignore removing on CI, keep for cleaning on local machines
	// purposely not checking the error here. The caller can check
	return func() {
		err := initializer.RemoveAll()
		if err != nil {
			panic(err)
		}
	}, err
}
 // Clone executes git clone in target directory and checksout the provided commit, or master if left ""
func Clone(dir string, repoURL string, commit string) error {
	repo, err := git.PlainClone(dir, false, &git.CloneOptions{
		URL:               repoURL,
		RecurseSubmodules: 1,
	})
	if err != nil && err.Error() != "repository already exists" {
		return err
	}
 	worktree, err := repo.Worktree()
	if err != nil {
		return err
	}
 	err = worktree.Checkout(&git.CheckoutOptions{
		Hash: plumbing.NewHash(commit),
	})
	if err != nil {
		return err
	}
 	return nil
}
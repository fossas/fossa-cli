package fixtures

import (
	"os"
	"path/filepath"
	"sync"

	"github.com/fossas/fossa-cli/files"
	git "gopkg.in/src-d/go-git.v4"
	"gopkg.in/src-d/go-git.v4/plumbing"
)

// ProjectFixture describes a git locator for a test fixture and env vars for when it is to be built
type ProjectFixture struct {
	Name   string
	URL    string
	Commit string
	Env    map[string]string
}

// FixtureDirectory returns the directory under which tests fixtures should be placed
func FixtureDirectory() string {
	return filepath.Join(os.TempDir(), "fossa-cli-fixtures")
}

// Clone executes git clone in target directory and checksout the provided commit, or master if left ""
func Clone(baseDir string, projects []ProjectFixture) error {
	baseDirExists, err := files.ExistsFolder(baseDir)
	if err != nil {
		return err
	}
	if baseDirExists {
		println(baseDir + "already exists, assuming that clone has already been executed")
		return nil
	}

	err = os.MkdirAll(baseDir, os.FileMode(0700))
	if err != nil {
		return err
	}

	var waitGroup sync.WaitGroup
	waitGroup.Add(len(projects))

	for _, project := range projects {
		go func(proj ProjectFixture) {
			defer waitGroup.Done()
			projectDir := filepath.Join(baseDir, proj.Name)

			repo, err := git.PlainClone(projectDir, false, &git.CloneOptions{
				URL:               proj.URL,
				RecurseSubmodules: 1,
			})
			if err != nil && err.Error() != "repository already exists" {
				panic(err)
			}

			worktree, err := repo.Worktree()
			if err != nil {
				panic(err)
			}
			err = worktree.Checkout(&git.CheckoutOptions{
				Hash: plumbing.NewHash(proj.Commit),
			})
			if err != nil {
				panic(err)
			}
		}(project)
	}

	waitGroup.Wait()

	return nil
}

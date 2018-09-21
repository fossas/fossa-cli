package testing

import (
	"os"
	"path/filepath"
	"sync"

	"github.com/fossas/fossa-cli/cmd/fossa/app"

	git "gopkg.in/src-d/go-git.v4"
	"gopkg.in/src-d/go-git.v4/plumbing"

	"github.com/fossas/fossa-cli/files"
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
		println(baseDir + "already exists, skipping initialization step")
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

// FossaInit effectively executes app.App.Run("init") within the provided directory
func FossaInit(projectDir string, apiKey string) error {
	// save the existing value of api key for any subsequent executions with different values (especially important if on CI)
	initialAPIKey := os.Getenv("FOSSA_API_KEY")
	err := os.Setenv("FOSSA_API_KEY", apiKey)
	if err != nil {
		return err
	}

	initialDir, err := os.Getwd()
	if err != nil {
		return err
	}

	err = os.Chdir(projectDir)
	if err != nil {
		return err
	}

	err = app.App.Run([]string{"init"})
	if err != nil {
		return err
	}

	// undo all tmp changes inside this test
	err = os.Setenv("FOSSA_API_KEY", initialAPIKey)
	if err != nil {
		return err
	}

	return os.Chdir(initialDir)
}

package fixtures

import (
	"os"
	"path/filepath"
	"sync"

	"github.com/apex/log"
	git "gopkg.in/src-d/go-git.v4"
	"gopkg.in/src-d/go-git.v4/plumbing"

	"github.com/fossas/fossa-cli/files"
)

// Project describes how to download, build, and analyse a fixture.
type Project struct {
	Name   string
	URL    string
	Commit string
	// Ref points to a particular reference. It will always be preferred over Commit
	Ref           string
	ModuleOptions map[string]interface{}
	Env           map[string]string
}

// Directory returns the path to test fixtures.
func Directory() string {
	return filepath.Join(os.TempDir(), "fossa-cli-fixtures")
}

// ProjectInitializer defines how a single project should be initialized after
// it has been cloned.
type ProjectInitializer func(p Project, projectDir string) error

// Initialize clones and initializes a fixture into a directory.
func Initialize(baseDir string, projects []Project, initializer ProjectInitializer) {
	// Exit early when fixtures already exist.
	// This is a bit scary since we don't validate whether the fixtures are
	// correct, but corrupted fixtures should cause erratic test failures. If we
	// wanted to validate fixtures, we might want to store e.g. a hash of the
	// fixture contents and verify against that (or validate the commit of the Git
	// repositories).
	baseDirExists, err := files.ExistsFolder(baseDir)
	if err == nil && baseDirExists {
		log.Debug(baseDir + " already exists, assuming fixtures OK")
		return
	}

	// Make the fixtures directory.
	err = os.MkdirAll(baseDir, os.FileMode(0700))
	if err != nil {
		panic(err)
	}

	// Clone and initialize in parallel.
	var waitGroup sync.WaitGroup
	waitGroup.Add(len(projects))
	for _, p := range projects {
		go func(project Project) {
			defer waitGroup.Done()

			projectDir := filepath.Join(baseDir, project.Name)
			if project.Ref != "" {
				_, err := git.PlainClone(projectDir, false, &git.CloneOptions{
					URL:               project.URL,
					Depth:             1,
					RecurseSubmodules: 1,
					ReferenceName:     plumbing.ReferenceName(project.Ref),
					SingleBranch:      true,
				})
				if err != nil {
					panic(err)
				}
			} else {
				repo, err := git.PlainClone(projectDir, false, &git.CloneOptions{
					URL:               project.URL,
					RecurseSubmodules: 1,
				})
				if err != nil {
					panic(err)
				}

				worktree, err := repo.Worktree()
				if err != nil {
					panic(err)
				}
				err = worktree.Checkout(&git.CheckoutOptions{
					Hash: plumbing.NewHash(project.Commit),
				})
				if err != nil {
					panic(err)
				}
			}

			err = initializer(project, projectDir)
			if err != nil {
				panic(err)
			}
		}(p)
	}

	waitGroup.Wait()
}

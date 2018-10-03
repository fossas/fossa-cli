package fixtures

import (
	"os"
	"path/filepath"
	"sync"

	"github.com/fossas/fossa-cli/files"
	git "gopkg.in/src-d/go-git.v4"
	"gopkg.in/src-d/go-git.v4/plumbing"
)

// Project describes a git repo for a test fixture and env vars for when it is to be built
type Project struct {
	Name    string
	URL     string
	Commit  string
	Options map[string]interface{}
	Env     map[string]string
}

// Directory returns the directory under which tests fixtures should be placed
func Directory() string {
	return filepath.Join(os.TempDir(), "fossa-cli-fixtures")
}

// Clone executes git clone in target directory and checksout the provided commit, or master if left ""
func Clone(baseDir string, projects []Project) error {
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
		go func(proj Project) {
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

// ProjectInitializerFunction defines how a single project should be initialized *after* it has already been cloned
type ProjectInitializerFunction func(proj Project, projectDir string) error

// InitializeProjects initializes provided projects in parallel using the provided initialization function. Errors are returned as a map of the Project.Name to the error thrown
func InitializeProjects(testDir string, projects []Project, projectInitializer ProjectInitializerFunction) map[string]error {
	var waitGroup sync.WaitGroup
	waitGroup.Add(len(projects))

	threadsafeErrorsByProject := threadableErrorsByProject{
		errorsByProject: make(map[string]error, len(projects)),
	}

	for _, project := range projects {
		go func(proj Project) {
			defer waitGroup.Done()
			threadsafeErrorsByProject.Lock()
			threadsafeErrorsByProject.errorsByProject[proj.Name] = projectInitializer(proj, filepath.Join(testDir, proj.Name))
			threadsafeErrorsByProject.Unlock()
		}(project)
	}
	waitGroup.Wait()

	return threadsafeErrorsByProject.errorsByProject
}

type threadableErrorsByProject struct {
	sync.Mutex
	errorsByProject map[string]error
}

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

// ProjectInitializerFunction defines how a single project should be initialized *after* it has already been cloned
type ProjectInitializerFunction func(proj Project, projectDir string) error

// Initialize executes git clone in target directory and checksout the provided commit, then runts the initializerFn. This is done asynchronously for each provided project.
// If the same fixture (keyed by baseDir) calls this function twice, the first call will run the initialization while the second waits for the first to complete. After the first
// completes, the second returns without doing any further work. This allows multiple tests for the same fixture to live within the same file without running into concurrency issues
func Initialize(baseDir string, projects []Project, initializerFn ProjectInitializerFunction) {
	globalLockMutex.Lock()
	// if the lock already exists, then that particular fixture has already begun initialization; wait until initialization is complete, then return
	if fixtureMutex, mutexExists := fixtureInitializerLocks[baseDir]; mutexExists {
		globalLockMutex.Unlock()
		// call Lock to cause the thread to wait until the execution is complete
		fixtureMutex.Lock()
		fixtureMutex.Unlock()

		return
	} else {
		currentFixtureLock := &sync.Mutex{}
		currentFixtureLock.Lock()
		defer currentFixtureLock.Unlock()

		fixtureInitializerLocks[baseDir] = currentFixtureLock
	}
	globalLockMutex.Unlock()

	baseDirExists, err := files.ExistsFolder(baseDir)
	if err != nil {
		panic(err)
	}
	if baseDirExists {
		println(baseDir + "already exists, assuming that clone has already been executed")
		return
	}

	err = os.MkdirAll(baseDir, os.FileMode(0700))
	if err != nil {
		panic(err)
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

			err = initializerFn(proj, projectDir)
			if err != nil {
				panic(err)
			}
		}(project)
	}

	waitGroup.Wait()
}

// Manages parallel access to the fixtureInitializerLocks. This prevents two integration tests with the same fixture from reading and writing fixtureInitializerLocks at the same time, potentially missing each other's execution
var globalLockMutex = sync.Mutex{}

// Used to determine which fixtures have started initialization and causes all future calls to the function to wait until initialization is complete, then skip it
var fixtureInitializerLocks = make(map[string]*sync.Mutex)

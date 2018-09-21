package nodejs_test

import (
	"flag"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	testUtils "github.com/fossas/fossa-cli/testing"
)

var nodeAnalyzerFixtureDir = filepath.Join(testUtils.FixtureDirectory(), "nodejs", "analyzer")

func TestMain(m *testing.M) {
	// flags are not parsed at this point. In order to have testing.Short() read actually provided values, this must be executed
	flag.Parse()
	if testing.Short() {
		return
	}

	err := testUtils.Clone(nodeAnalyzerFixtureDir, projects)
	if err != nil {
		panic(err)
	}

	println("projects cloned, starting initialization")

	err = initializeProjects(nodeAnalyzerFixtureDir)
	if err != nil {
		panic(err)
	}

	println("Initialization complete, running tests")

	exitCode := m.Run()
	defer os.Exit(exitCode)
	// defer cleanUp(nodeAnalyzerFixtureDir)
}

// While not testing the core functionality, this ensures that the tests have been setup correctly as needed for a prereq to run the analyzer steps
// This test itself does not incur any overhead.
func TestTestSetup(t *testing.T) {
	t.Parallel()
	assertProjectFixtureExists(t, "puppeteer")
	// faker has no deps
	// assertProjectFixtureExists(t, "fakerjs")
	assertProjectFixtureExists(t, "fastify")
	assertProjectFixtureExists(t, "nest")
	assertProjectFixtureExists(t, "ohm")
	assertProjectFixtureExists(t, "express")
	assertProjectFixtureExists(t, "standard")
	assertProjectFixtureExists(t, "sodium-encryption")
	assertProjectFixtureExists(t, "request")
}

func TestAnalysisOutput(t *testing.T) {
	t.Parallel()
	for _, proj := range projects {
		t.Run(proj.Name, func(t *testing.T) {
			t.Parallel()
			module := module.Module{
				Dir:         filepath.Join(nodeAnalyzerFixtureDir, proj.Name),
				Type:        pkg.NodeJS,
				Name:        "standardjs",
				Options:     map[string]interface{}{"allow-npm-err": true},
				BuildTarget: filepath.Join(nodeAnalyzerFixtureDir, proj.Name, "package.json"),
			}

			analyzer, err := analyzers.New(module)
			assert.NoError(t, err)

			deps, err := analyzer.Analyze()
			assert.NoError(t, err)
			// faker has no deps
			if proj.Name == "fakerjs" {
				assert.Empty(t, deps.Direct)
				assert.Empty(t, deps.Transitive)
			} else {
				assert.NotEmpty(t, deps.Direct)
				assert.NotEmpty(t, deps.Transitive)
			}
		})
	}
}

func assertProjectFixtureExists(t *testing.T, name string) {
	exists, err := files.ExistsFolder(nodeAnalyzerFixtureDir, name)
	assert.NoError(t, err)
	assert.True(t, exists, name+" was not properly cloned")

	exists, err = files.ExistsFolder(nodeAnalyzerFixtureDir, name, "node_modules")
	assert.NoError(t, err)
	assert.True(t, exists, name+" did not have its node modules installed")
}

func initializeProjects(testDir string) error {
	var waitGroup sync.WaitGroup
	waitGroup.Add(len(projects))

	for _, project := range projects {
		go func(proj testUtils.ProjectFixture) {
			defer waitGroup.Done()
			startTime := time.Now()
			println("installing " + proj.Name)
			projectDir := filepath.Join(testDir, proj.Name)

			_, errOut, err := exec.Run(exec.Cmd{
				Name:    "npm",
				Argv:    []string{"install", "--production"},
				Dir:     projectDir,
				WithEnv: proj.Env,
				Command: "npm",
			})
			if err != nil {
				println(errOut)
				println("failed to run npm install on " + proj.Name)
			}

			elapsedTime := time.Since(startTime)
			println(proj.Name, " install time: ", elapsedTime.Seconds(), "s")

			// save time on local
			ymlAlreadyExists, err := files.Exists(filepath.Join(projectDir, ".fossa.yml"))
			if err != nil {
				panic(err)
			}
			if ymlAlreadyExists {
				return
			}

			println(proj.Name, " starting fossa init")
			startTime = time.Now()
			_, err = testUtils.FossaInit(projectDir)
			if err != nil {
				println("failed to run fossa init on " + proj.Name)
				println(err.Error())
				panic(err)
			}
			elapsedTime = time.Since(startTime)
			println(proj.Name, " init time: ", elapsedTime.Seconds(), "s")
		}(project)
	}

	waitGroup.Wait()

	return nil
}

var projects = []testUtils.ProjectFixture{
	testUtils.ProjectFixture{
		Name:   "puppeteer",
		URL:    "https://github.com/GoogleChrome/puppeteer",
		Commit: "b97bddf8e5750d20c6ba82392eebe2a3fd2dd218",
		Env: map[string]string{
			"PUPPETEER_SKIP_CHROMIUM_DOWNLOAD": "1",
		},
	},
	testUtils.ProjectFixture{
		Name:   "fakerjs",
		URL:    "https://github.com/Marak/faker.js",
		Commit: "3a4bb358614c1e1f5d73f4df45c13a1a7aa013d7",
		Env:    map[string]string{},
	},
	testUtils.ProjectFixture{
		Name:   "fastify",
		URL:    "https://github.com/fastify/fastify",
		Commit: "1b16a4c5e381f9292d3ac2c327c3bda4bd277408",
		Env:    map[string]string{},
	},
	testUtils.ProjectFixture{
		Name:   "nest",
		URL:    "https://github.com/nestjs/nest",
		Commit: "ce498e86150f7de4a260f0c393d47ec4cc920ea1",
		Env:    map[string]string{},
	},
	testUtils.ProjectFixture{
		Name:   "ohm",
		URL:    "https://github.com/harc/ohm",
		Commit: "8202eff3723cfa26522134e7b003cf31ab5de445",
		Env:    map[string]string{},
	},
	testUtils.ProjectFixture{
		Name:   "express",
		URL:    "https://github.com/expressjs/express",
		Commit: "b4eb1f59d39d801d7365c86b04500f16faeb0b1c",
		Env:    map[string]string{},
	},
	testUtils.ProjectFixture{
		Name:   "standard",
		URL:    "https://github.com/standard/standard",
		Commit: "bc02256fa2c03632e657248483c55a752e63e724",
		Env:    map[string]string{},
	},
	testUtils.ProjectFixture{
		Name:   "sodium-encryption",
		URL:    "https://github.com/mafintosh/sodium-encryption",
		Commit: "42a7cba0f97718157e8c7a386ef94ba31e16837a",
		Env:    map[string]string{},
	},
	testUtils.ProjectFixture{
		Name:   "request",
		URL:    "https://github.com/request/request",
		Commit: "8162961dfdb73dc35a5a4bfeefb858c2ed2ccbb7",
		Env:    map[string]string{},
	},
}

func cleanUp(dir string) {
	err := os.RemoveAll(dir)
	if err != nil {
		panic(err)
	}
}

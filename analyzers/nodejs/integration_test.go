package nodejs_test

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/apex/log"
	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/fossas/fossa-cli/testing/fixtures"
	"github.com/fossas/fossa-cli/testing/runfossa"
)

var nodeAnalyzerFixtureDir = filepath.Join(fixtures.Directory(), "nodejs", "analyzer")

func TestNodejsAnalysis(t *testing.T) {
	if testing.Short() {
		t.Skip("Integration tests to not run with the -short test flag")
		return
	}
	t.Parallel()

	fixtures.Initialize(nodeAnalyzerFixtureDir, projects, projectInitializer)

	for _, project := range projects {
		proj := project
		t.Run("Analysis: "+proj.Name, func(t *testing.T) {
			t.Parallel()

			module := module.Module{
				Dir:         filepath.Join(nodeAnalyzerFixtureDir, proj.Name),
				Type:        pkg.NodeJS,
				Name:        proj.Name,
				Options:     proj.Options,
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

func projectInitializer(proj fixtures.Project, projectDir string) error {
	nodeModulesExist, err := files.ExistsFolder(projectDir, "node_modules")
	if err != nil {
		return err
	}

	if nodeModulesExist {
		log.Debug("node_modules already exists for " + proj.Name + "skipping initialization")
		return nil
	}

	_, errOut, err := exec.Run(exec.Cmd{
		Name:    "npm",
		Argv:    []string{"install", "--production"},
		Dir:     projectDir,
		WithEnv: proj.Env,
		Command: "npm",
	})
	if err != nil {
		log.Error(errOut)
		log.Error("failed to run npm install on " + proj.Name)
		return err
	}

	ymlAlreadyExists, err := files.Exists(filepath.Join(projectDir, ".fossa.yml"))
	if err != nil {
		return err
	}
	if ymlAlreadyExists {
		return nil
	}

	stdout, stderr, err := runfossa.Init(projectDir)
	if err != nil {
		log.Error("failed to run fossa init on " + proj.Name)
		log.Error(stdout)
		log.Error(stderr)
		return err
	}

	return nil
}

var projects = []fixtures.Project{
	fixtures.Project{
		Name:   "puppeteer",
		URL:    "https://github.com/GoogleChrome/puppeteer",
		Commit: "b97bddf8e5750d20c6ba82392eebe2a3fd2dd218",
		Env: map[string]string{
			"PUPPETEER_SKIP_CHROMIUM_DOWNLOAD": "1",
		},
	},
	fixtures.Project{
		Name:   "fakerjs",
		URL:    "https://github.com/Marak/faker.js",
		Commit: "3a4bb358614c1e1f5d73f4df45c13a1a7aa013d7",
	},
	fixtures.Project{
		Name:   "fastify",
		URL:    "https://github.com/fastify/fastify",
		Commit: "1b16a4c5e381f9292d3ac2c327c3bda4bd277408",
	},
	fixtures.Project{
		Name:   "nest",
		URL:    "https://github.com/nestjs/nest",
		Commit: "ce498e86150f7de4a260f0c393d47ec4cc920ea1",
	},
	fixtures.Project{
		Name:   "ohm",
		URL:    "https://github.com/harc/ohm",
		Commit: "8202eff3723cfa26522134e7b003cf31ab5de445",
	},
	fixtures.Project{

		Name:   "express",
		URL:    "https://github.com/expressjs/express",
		Commit: "b4eb1f59d39d801d7365c86b04500f16faeb0b1c",
	},
	fixtures.Project{
		Name:    "standard",
		URL:     "https://github.com/standard/standard",
		Commit:  "bc02256fa2c03632e657248483c55a752e63e724",
		Options: map[string]interface{}{"allow-npm-err": true},
	},
	fixtures.Project{
		Name:   "sodium-encryption",
		URL:    "https://github.com/mafintosh/sodium-encryption",
		Commit: "42a7cba0f97718157e8c7a386ef94ba31e16837a",
	},
	fixtures.Project{
		Name:   "request",
		URL:    "https://github.com/request/request",
		Commit: "8162961dfdb73dc35a5a4bfeefb858c2ed2ccbb7",
	},
}

func cleanUp(dir string) {
	err := os.RemoveAll(dir)
	if err != nil {
		panic(err)
	}
}

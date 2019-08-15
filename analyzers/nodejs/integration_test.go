package nodejs_test

import (
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

func TestNodejsIntegration(t *testing.T) {
	if testing.Short() {
		t.Skip("Integration tests to not run with the -short test flag")
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
				Options:     proj.ModuleOptions,
				BuildTarget: filepath.Join(nodeAnalyzerFixtureDir, proj.Name),
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
		Name:   "fakerjs",
		URL:    "https://github.com/Marak/faker.js",
		Commit: "3a4bb358614c1e1f5d73f4df45c13a1a7aa013d7",
	},
	fixtures.Project{
		Name:   "nest",
		URL:    "https://github.com/nestjs/nest",
		Commit: "ce498e86150f7de4a260f0c393d47ec4cc920ea1",
	},
	fixtures.Project{

		Name:   "express",
		URL:    "https://github.com/expressjs/express",
		Commit: "b4eb1f59d39d801d7365c86b04500f16faeb0b1c",
	},
	fixtures.Project{
		Name:          "standard",
		URL:           "https://github.com/standard/standard",
		Commit:        "bc02256fa2c03632e657248483c55a752e63e724",
		ModuleOptions: map[string]interface{}{"allow-npm-err": true},
	},
}

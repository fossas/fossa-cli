package python_test

import (
	"flag"
	"os"
	"path/filepath"
	"testing"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/fossas/fossa-cli/testing/fixtures"
	"github.com/stretchr/testify/assert"
)

var pythonAnalyzerFixtureDir = filepath.Join(fixtures.Directory(), "python", "analyzer")

func TestMain(m *testing.M) {
	// flags are not parsed at this point. In order to have testing.Short() read actually provided values, this must be executed
	flag.Parse()
	if testing.Short() {
		return
	}

	err := fixtures.Clone(pythonAnalyzerFixtureDir, projects)
	if err != nil {
		panic(err)
	}

	sentryLocation := filepath.Join(pythonAnalyzerFixtureDir, "sentry")
	stdout, stdErr, err := exec.Run(exec.Cmd{
		Name:    "pip",
		Argv:    []string{"install", "-r", "requirements-base.txt"},
		Dir:     sentryLocation,
		Command: "pip",
	})
	if err != nil {
		log.Error(stdout)
		log.Error(stdErr)
		panic(err)
	}
	_, stdErr, err = exec.Run(exec.Cmd{
		Name:    "pip",
		Argv:    []string{"install", "-r", "requirements-dev.txt"},
		Dir:     sentryLocation,
		Command: "pip",
	})
	if err != nil {
		log.Error(stdErr)
		panic(err)
	}

	exitCode := m.Run()
	defer os.Exit(exitCode)
}

func TestPythonAnalysis(t *testing.T) {
	t.Parallel()
	for _, project := range projects {
		proj := project
		projDir := filepath.Join(pythonAnalyzerFixtureDir, proj.Name)
		t.Run("Analysis:"+proj.Name, func(t *testing.T) {
			t.Parallel()
			module := module.Module{
				Dir:         projDir,
				Type:        pkg.Python,
				Name:        proj.Name,
				Options:     map[string]interface{}{"strategy": "deptree"},
				BuildTarget: projDir,
			}

			if proj.Name == "sentry" {
				module.Options["strategy"] = "deptree"
			}

			analyzer, err := analyzers.New(module)
			assert.NoError(t, err)

			deps, err := analyzer.Analyze()
			assert.NoError(t, err)

			assert.NotEmpty(t, deps.Direct)
			assert.NotEmpty(t, deps.Transitive)
		})
	}
}

var projects = []fixtures.Project{
	fixtures.Project{
		Name:   "vibora",
		URL:    "https://github.com/vibora-io/vibora",
		Commit: "51d6691a430bbb6ac57df54dc7d8276f62953bb2",
	},
	// ansible is failing checkout to a commit with the error: worktree contains unstaged changes, but I cannot figure out why
	// fixtures.Project{
	// 	Name:   "ansible",
	// 	URL:    "https://github.com/ansible/ansible",
	// 	Commit: "649403c3a179277ec2ad7373962cb7baee2f715f",
	// },
	fixtures.Project{
		Name:   "sentry",
		URL:    "https://github.com/getsentry/sentry",
		Commit: "985a917353d23caa112d133cfa6873389d831be5",
	},
	fixtures.Project{
		Name:   "fabric",
		URL:    "https://github.com/fabric/fabric",
		Commit: "c37df4cae043eee3b6c5fc39a98d72437b45a938",
	},
	fixtures.Project{
		Name:   "boto",
		URL:    "https://github.com/boto/boto",
		Commit: "eca5f987aa59ec7490aa91466853c36b0cf40b3c",
	},
	fixtures.Project{
		Name:   "NewsBlur",
		URL:    "https://github.com/samuelclay/NewsBlur",
		Commit: "121daffcdae4bc324cf6e81f095e28529e81372d",
	},
	fixtures.Project{
		Name:   "legit",
		URL:    "https://github.com/kennethreitz/legit",
		Commit: "20187b45ce4d32146d2a2d493c315924c1cc8287",
	},
	fixtures.Project{
		Name:   "sshuttle",
		URL:    "https://github.com/sshuttle/sshuttle",
		Commit: "6dc368bde8128cd27ad80d48772420ff68a92c8f",
	},
}

package gradle_test

import (
	"flag"
	"os"
	"path/filepath"
	"testing"

	"github.com/apex/log"
	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/fossas/fossa-cli/testing/fixtures"
	"github.com/fossas/fossa-cli/testing/runfossa"
)

var gradleAnalyzerFixtureDir = filepath.Join(fixtures.Directory(), "gradle", "analyzer")

func TestMain(m *testing.M) {
	// flags are not parsed at this point. In order to have testing.Short() read actually provided values, this must be executed
	flag.Parse()
	if testing.Short() {
		return
	}

	// err := fixtures.Clone(pythonAnalyzerFixtureDir, projects)
	fixtures.Initialize(gradleAnalyzerFixtureDir, projects, func(proj fixtures.Project, projectDir string) error {
		stdout, stderr, err := exec.Run(exec.Cmd{
			Argv:    []string{"-c", "source " + os.Getenv("HOME") + "/.sdkman/bin/sdkman-init.sh"},
			Name:    "bash",
			Command: "bash",
			Dir:     projectDir,
		})
		if err != nil {
			log.Error(stderr)
			panic(err)
		}

		if proj.Name == "iosched" {
			stdout, stderr, err = exec.Run(exec.Cmd{
				Argv:    []string{"-c", "yes | sdkmanager --licenses"},
				Name:    "bash",
				Command: "bash",
				Dir:     projectDir,
			})
			if err != nil {
				log.Error(stderr)
				panic(err)
			}
		}

		stdout, stderr, err = exec.Run(exec.Cmd{
			Name:    "gradlew",
			Command: "gradlew",
			Argv:    []string{"./gradlew"},
			Dir:     projectDir,
		})
		if err != nil {
			log.Error(stderr)
			panic(err)
		}

		stdout, stderr, err = runfossa.Init(projectDir)
		if err != nil {
			log.Error("failed to run fossa init on " + proj.Name)
			log.Error(stdout)
			log.Error(stderr)
			return err
		}

		return nil
	})

	exitCode := m.Run()
	defer os.Exit(exitCode)
}

func TestGradleAnalysis(t *testing.T) {
	t.Parallel()
	for _, project := range projects {
		proj := project
		projDir := filepath.Join(gradleAnalyzerFixtureDir, proj.Name)
		t.Run("Analysis:"+proj.Name, func(t *testing.T) {
			t.Parallel()
			module := module.Module{
				Dir:         projDir,
				Type:        pkg.Python,
				Name:        proj.Name,
				Options:     map[string]interface{}{},
				BuildTarget: projDir,
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
		Name:   "hibernate-orm",
		URL:    "https://github.com/hibernate/hibernate-orm",
		Commit: "1db476dbd5ec4f852472b953d50a8caa073efd6d",
	},
	fixtures.Project{
		Name:   "iosched",
		URL:    "https://github.com/google/iosched",
		Commit: "bf8e50af6f1487e7cd5cec1ff510750ab207e293",
	},
}

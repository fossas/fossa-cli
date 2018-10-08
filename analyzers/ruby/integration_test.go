package ruby_test

import (
	"flag"
	"os"
	"path/filepath"
	"testing"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/fossas/fossa-cli/testing/fixtures"
	"github.com/fossas/fossa-cli/testing/runfossa"
	"github.com/stretchr/testify/assert"
)

var rubyAnalyzerFixtureDir = filepath.Join(fixtures.Directory(), "ruby", "analyzer")

func TestMain(m *testing.M) {
	// flags are not parsed at this point. In order to have testing.Short() read actually provided values, this must be executed
	flag.Parse()
	if testing.Short() {
		return
	}

	fixtures.Initialize(rubyAnalyzerFixtureDir, projects, func(proj fixtures.Project, projectDir string) error {
		ymlAlreadyExists, err := files.Exists(filepath.Join(projectDir, ".fossa.yml"))
		if err != nil {
			panic(err)
		}
		if ymlAlreadyExists {
			return nil
		}

		args := []string{"install"}

		// we could extend or refactor the fixtures.Project struct, but because this is a single case, this is simpler for the time being
		if proj.Name == "rails" {
			args = append(args, []string{"--deployment", "--without", "doc", "job", "cable", "storage", "ujs", "test", "db"}...)
		}

		_, stderr, err := exec.Run(exec.Cmd{
			Command: "bundle",
			Name:    "bundle",
			Argv:    args,
			Dir:     projectDir,
		})
		if err != nil {
			log.Error("failed to run fossa init on " + proj.Name)
			log.Error(stderr)
			return err
		}

		// any key will work to prevent the "NEED KEY" error message
		_, stderr, err = runfossa.Init(projectDir)
		if err != nil {
			log.Error("failed to run fossa init on " + proj.Name)
			log.Error(stderr)
			return err
		}

		return nil
	})

	exitCode := m.Run()
	defer os.Exit(exitCode)
}

func TestAnalylze(t *testing.T) {
	t.Parallel()
	for _, project := range projects {
		proj := project
		t.Run("Analysis:"+proj.Name, func(t *testing.T) {
			t.Parallel()

			module := module.Module{
				Dir:         filepath.Join(rubyAnalyzerFixtureDir, proj.Name),
				Type:        pkg.Ruby,
				Name:        proj.Name,
				Options:     proj.Options,
				BuildTarget: filepath.Join(rubyAnalyzerFixtureDir, proj.Name),
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
	// branch 5-2-stable
	fixtures.Project{
		Name:   "rails",
		URL:    "https://github.com/rails/rails",
		Commit: "3a875402189f0d42446963ff0322f413f892d544",
	},
	// release tag v2.1.5
	// fixtures.Project{
	// 	Name:   "vagrant",
	// 	URL:    "https://github.com/hashicorp/vagrant",
	// 	Commit: "47b2f27d2728e56ac1d3d8bb78bf34b1f9956056",
	// },
}

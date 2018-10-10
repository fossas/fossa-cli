package ruby_test

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

var rubyAnalyzerFixtureDir = filepath.Join(fixtures.Directory(), "ruby", "analyzer")

func TestRubyIntegration(t *testing.T) {
	if testing.Short() {
		return
	}
	t.Parallel()

	fixtures.Initialize(rubyAnalyzerFixtureDir, projects, projectInitializer)
	for _, project := range projects {
		proj := project
		t.Run("Analysis:\t"+proj.Name, func(t *testing.T) {
			t.Parallel()

			module := module.Module{
				Dir:         filepath.Join(rubyAnalyzerFixtureDir, proj.Name),
				Type:        pkg.Ruby,
				Name:        proj.Name,
				Options:     proj.ModuleOptions,
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

func projectInitializer(proj fixtures.Project, projectDir string) error {
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
}

var projects = []fixtures.Project{
	fixtures.Project{
		Name: "rails",
		URL:  "https://github.com/rails/rails",
		Ref:  "refs/tags/v5.2.1",
	},
	fixtures.Project{
		Name: "vagrant",
		URL:  "https://github.com/hashicorp/vagrant",
		Ref:  "refs/tags/v2.1.5",
	},
}

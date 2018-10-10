package python_test

import (
	"path/filepath"
	"testing"

	"github.com/apex/log"
	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/fossas/fossa-cli/testing/fixtures"
	"github.com/fossas/fossa-cli/testing/runfossa"
)

var pythonAnalyzerFixtureDir = filepath.Join(fixtures.Directory(), "python", "analyzer")

func TestPythonIntegration(t *testing.T) {
	if testing.Short() {
		t.Skip("Integration tests to not run with the -short test flag")
	}
	t.Parallel()

	// err := fixtures.Clone(pythonAnalyzerFixtureDir, projects)
	fixtures.Initialize(pythonAnalyzerFixtureDir, projects, func(proj fixtures.Project, projectDir string) error {
		stdout, stderr, err := runfossa.Init(projectDir)
		if err != nil {
			log.Error("failed to run fossa init on " + proj.Name)
			log.Error(stdout)
			log.Error(stderr)
			return err
		}

		return nil
	})
	for _, project := range projects {
		proj := project
		projDir := filepath.Join(pythonAnalyzerFixtureDir, proj.Name)
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
		Name:   "vibora",
		URL:    "https://github.com/vibora-io/vibora",
		Commit: "51d6691a430bbb6ac57df54dc7d8276f62953bb2",
	},
	// sentry seg faults on pip install. This is linked to the particular commit. This can be resolved down the line once native integration tests are stable
	// fixtures.Project{
	// 	Name:   "sentry",
	// 	URL:    "https://github.com/getsentry/sentry",
	// 	Commit: "985a917353d23caa112d133cfa6873389d831be5",
	// },
	// The original fixture docker image contained the projects listed below, however the test.sh file did not test them
	// For this reason, these will remain untested until we reach test parity with the previous integraiton tests.
	// TODO: support the fixture cases listed below. Note that the circle cache number will need to be updated for this to take effect
	// fixtures.Project{
	// 	Name:   "ansible",
	// 	URL:    "https://github.com/ansible/ansible",
	// 	Commit: "649403c3a179277ec2ad7373962cb7baee2f715f",
	// },
	// fixtures.Project{
	// 	Name:   "fabric",
	// 	URL:    "https://github.com/fabric/fabric",
	// 	Commit: "c37df4cae043eee3b6c5fc39a98d72437b45a938",
	// },
	// fixtures.Project{
	// 	Name:   "boto",
	// 	URL:    "https://github.com/boto/boto",
	// 	Commit: "eca5f987aa59ec7490aa91466853c36b0cf40b3c",
	// },
	// fixtures.Project{
	// 	Name:   "NewsBlur",
	// 	URL:    "https://github.com/samuelclay/NewsBlur",
	// 	Commit: "121daffcdae4bc324cf6e81f095e28529e81372d",
	// },
	// fixtures.Project{
	// 	Name:   "legit",
	// 	URL:    "https://github.com/kennethreitz/legit",
	// 	Commit: "20187b45ce4d32146d2a2d493c315924c1cc8287",
	// },
	// fixtures.Project{
	// 	Name:   "sshuttle",
	// 	URL:    "https://github.com/sshuttle/sshuttle",
	// 	Commit: "6dc368bde8128cd27ad80d48772420ff68a92c8f",
	// },
}

package gradle_test

import (
	"path/filepath"
	"testing"

	"github.com/apex/log"
	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/testing/fixtures"
	"github.com/fossas/fossa-cli/testing/runfossa"
)

var (
	fixtureDir = filepath.Join(fixtures.Directory(), "gradle", "analyzer")
	grpc       = fixtures.Project{
		Name:   "grpc-java",
		URL:    "https://github.com/grpc/grpc-java",
		Commit: "cf083239e7cbbde7e8ed7d3f038202735f84c2d3",
	}
	projects = []fixtures.Project{grpc}
)

func TestGradleIntegration(t *testing.T) {
	if testing.Short() {
		t.Skip("Skip integration test")
	}

	fixtures.Initialize(fixtureDir, projects, func(p fixtures.Project, dir string) error {
		args := []string{"build", "-p", dir}
		command := filepath.Join(dir, "gradlew")
		_, stderr, err := exec.Run(exec.Cmd{
			Name: command,
			Argv: args,
		})
		if err != nil {
			log.Error("Error running ./gradlew")
			log.Error(stderr)
		}

		_, _, err = runfossa.Init(dir)
		assert.NoError(t, err)

		return nil
	})

	targets := map[string]string{
		grpc.Name: "gradle:grpc-netty",
	}

	for project, target := range targets {
		output, err := runfossa.AnalyzeOutput(filepath.Join(fixtureDir, project), []string{target})
		assert.NoError(t, err)
		assert.NotEmpty(t, output)
		assert.NotEmpty(t, output[0].Build.Dependencies)
		assert.NotEmpty(t, output[0].Build.Imports)
		t.Logf("%#v", output)
	}
}

package gradle_test

import (
	"path/filepath"
	"testing"

	"github.com/apex/log"
	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/testing/fixtures"
	"github.com/fossas/fossa-cli/testing/runfossa"
)

var (
	fixtureDir = filepath.Join(fixtures.Directory(), "gradle", "analyzer")
	project    = fixtures.Project{
		Name:   "grpc-java",
		URL:    "https://github.com/grpc/grpc-java",
		Commit: "cf083239e7cbbde7e8ed7d3f038202735f84c2d3",
	}
)

func TestGradleIntegration(t *testing.T) {
	if testing.Short() {
		t.Skip("Skip integration test")
	}

	fixtures.Initialize(fixtureDir, []fixtures.Project{project}, func(p fixtures.Project, dir string) error {

		_, stderr, err := runfossa.Init(dir)
		if err != nil {
			log.Error("failed to run fossa init on " + p.Name)
			log.Error(stderr)
			return err
		}

		return nil
	})

	targets := []string{
		"gradle:grpc-netty",
	}

	for _, target := range targets {
		output, err := runfossa.AnalyzeOutput(dir, []string{target})
		assert.NoError(t, err)
		assert.NotEmpty(t, output)
		assert.NotEmpty(t, output[0].Build.Dependencies)
		assert.NotEmpty(t, output[0].Build.Imports)
		t.Logf("%#v", output)
	}
}

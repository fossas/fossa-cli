package gradle_test

import (
	"fmt"
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

	fmt.Println(fixtureDir)
	fixtures.Initialize(fixtureDir, []fixtures.Project{project}, func(p fixtures.Project, dir string) error {

		fmt.Println("running gradle build")
		args := []string{"build"}
		_, stderr, err := exec.Run(exec.Cmd{
			Command: "./gradlew",
			Name:    "./gradlew",
			Argv:    args,
			Dir:     dir,
		})
		if err != nil {
			log.Error("Error running ./gradlew")
			log.Error(stderr)
		}
		return nil
	})

	dir := filepath.Join(fixtureDir, project.Name)
	out, e, err := runfossa.Init(dir)
	assert.NoError(t, err)
	fmt.Println(out, e)

	targets := []string{
		"gradle:grpc-netty",
	}

	for _, target := range targets {
		output, err := runfossa.AnalyzeOutput(dir, []string{target})
		fmt.Println(output)
		assert.NoError(t, err)
		assert.NotEmpty(t, output)
		assert.NotEmpty(t, output[0].Build.Dependencies)
		assert.NotEmpty(t, output[0].Build.Imports)
		t.Logf("%#v", output)
	}
}

package maven_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/maven"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

func TestCustomMavenPOMFile(t *testing.T) {
	buildTarget := "github.com/fossas/fossa-cli/cmd/fossa"
	m := module.Module{
		Name:        "test",
		Type:        pkg.Maven,
		BuildTarget: buildTarget,
		Dir:         "testdata",
	}

	mavenModule := m

	// what should I test here?
	// I need to know whether the command that I run includes the test dependency or not

	analyzer, err := maven.New(mavenModule)
	assert.NoError(t, err)

	analyzed, err := analyzer.Analyze()
	assert.NoError(t, err)

	// convert it into a string
	stringifiedAnalyzed := fmt.Sprintf("%#v", analyzed)

	assert.Contains(t, stringifiedAnalyzed, "com.amazonaws")
	assert.NotContains(t, stringifiedAnalyzed, "junit")
}

package gradle_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/gradle"
	"github.com/fossas/fossa-cli/graph"
)

func TestGradle(t *testing.T) {
	mock := MockGradle{Echo: "hi"}
	a := gradle.Analyzer{
		Setup: mock,
	}
	graph, err := a.Analyze()
	assert.NoError(t, err)
	fmt.Println(graph)
}

type MockGradle struct {
	Echo string
}

func (m MockGradle) DependenciesTask(temp string, args ...string) (map[string]graph.Deps, error) {
	return nil, nil
}

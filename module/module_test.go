package module_test

import (
	"context"
	"fmt"
	"testing"

	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/stretchr/testify/assert"
	"golang.org/x/sync/semaphore"
)

func TestOptimalAndSortedStrategies(t *testing.T) {
	test := module.AnalyzerV2{
		Name: "testing",
		Strategies: module.Strategies{
			Named: map[module.StrategyName]module.Strategy{
				"one": func(module.Filepath, module.Filepath) (graph.Deps, *errors.Error) {
					return graph.Deps{}, &errors.Error{Cause: fmt.Errorf("one failed")}
				},
				"two": func(module.Filepath, module.Filepath) (graph.Deps, *errors.Error) {
					return graph.Deps{}, nil
				},
				"three": func(module.Filepath, module.Filepath) (graph.Deps, *errors.Error) {
					return graph.Deps{}, nil
				},
				"four": func(module.Filepath, module.Filepath) (graph.Deps, *errors.Error) {
					return graph.Deps{}, &errors.Error{Cause: fmt.Errorf("four failed")}
				},
				"five": func(module.Filepath, module.Filepath) (graph.Deps, *errors.Error) {
					return graph.Deps{}, nil
				},
			},
			SortedNames: []module.StrategyName{"one", "two", "three", "four", "five"},
			Optimal:     []module.StrategyName{"one", "two"},
		},
	}

	disp := display.StartDisplay()
	defer disp.Stop()
	ctx := context.Background()
	sem := semaphore.NewWeighted(int64(1))
	startJob := func() error {
		return sem.Acquire(ctx, 1)
	}
	endJob := func() {
		sem.Release(1)
	}
	progress := disp.StartProcess(test.Name)

	strategiesToScan := map[string]string{
		"four":    "file-four",
		"two":     "file-two",
		"three":   "file-three",
		"one":     "file-one",
		"unknown": "unknown",
	}

	graphs, err := test.ScanModule(startJob, endJob, progress, ".", strategiesToScan)
	assert.Nil(t, err)
	expectedGraphOrder := []module.TaggedGraph{module.TaggedGraph{Strategy: "two", File: "file-two"}, module.TaggedGraph{Strategy: "three", File: "file-three"}}
	assert.Equal(t, expectedGraphOrder, graphs)
}

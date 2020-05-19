package golang_test

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/golang"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

func TestGoModWithoutGoPath(t *testing.T) {
	if testing.Short() {
		t.Skip("Skip integration test")
	}

	// Unset $GOPATH.
	prevGOPATH := os.Getenv("GOPATH")
	defer func() {
		os.Setenv("GOPATH", prevGOPATH)
	}()
	err := os.Unsetenv("GOPATH")
	assert.NoError(t, err)

	// Change to test directory.
	err = os.Chdir(filepath.Join("testdata", "gomod"))
	assert.NoError(t, err)
	defer func() {
		err = os.Chdir(filepath.Join("..", ".."))
		assert.NoError(t, err)
	}()

	// Run analysis.
	m := module.Module{
		Name:        "github.com/fossas/fossa-cli/analyzers/golang/testdata/gomod/cmd/gomodcmd",
		Type:        pkg.Go,
		BuildTarget: "github.com/fossas/fossa-cli/analyzers/golang/testdata/gomod/cmd/gomodcmd",
		Dir:         "cmd/gomodcmd",
	}
	a, err := golang.New(m)
	assert.NoError(t, err)

	output, err := a.Analyze()
	assert.NoError(t, err)
	assert.NotEmpty(t, output)
	t.Logf("%#v", output)
}

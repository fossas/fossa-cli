package pip_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/pip"
)

func TestFromFile(t *testing.T) {
	reqs, err := pip.FromFile("testdata/requirements.txt")
	assert.NoError(t, err)
	assert.Equal(t, 5, len(reqs))
	assert.Contains(t, reqs, pip.Requirement{Name: "simple", Revision: "1.0.0", Operator: "=="})
	assert.Contains(t, reqs, pip.Requirement{Name: "extra", Revision: "2.0.0", Operator: "=="})
	assert.Contains(t, reqs, pip.Requirement{Name: "latest"})
	assert.Contains(t, reqs, pip.Requirement{Name: "latestExtra"})
	assert.Contains(t, reqs, pip.Requirement{Name: "notEqualOp", Revision: "3.0.0", Operator: ">="})
	assert.NotContains(t, reqs, pip.Requirement{Name: "-r other-requirements.txt"})
	assert.NotContains(t, reqs, pip.Requirement{Name: "--option test-option"})
}

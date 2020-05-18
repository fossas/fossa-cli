package pip_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/pip"
)

func TestFromFile(t *testing.T) {
	reqs, err := pip.FromFile("testdata/requirements.txt")
	assert.Nil(t, err)
	assert.Equal(t, 8, len(reqs))
	assert.Contains(t, reqs, pip.Requirement{Name: "simple", Constraints: []pip.Constraint{{Revision: "1.0.0", Operator: "=="}}})
	assert.Contains(t, reqs, pip.Requirement{Name: "extra", Constraints: []pip.Constraint{{Revision: "2.0.0", Operator: "=="}}})
	assert.Contains(t, reqs, pip.Requirement{Name: "latest"})
	assert.Contains(t, reqs, pip.Requirement{Name: "latestExtra"})
	assert.Contains(t, reqs, pip.Requirement{Name: "notEqualOp", Constraints: []pip.Constraint{{Revision: "3.0.0", Operator: ">="}}})
	assert.Contains(t, reqs, pip.Requirement{Name: "comment-version", Constraints: []pip.Constraint{{Revision: "2.0.0", Operator: "==="}}})
	assert.Contains(t, reqs, pip.Requirement{Name: "comment"})
	assert.Contains(t, reqs, pip.Requirement{Name: "tilde", Constraints: []pip.Constraint{{Revision: "2.0.0", Operator: "~="}}})
	assert.NotContains(t, reqs, pip.Requirement{Name: "-r other-requirements.txt"})
	assert.NotContains(t, reqs, pip.Requirement{Name: "--option test-option"})
}

func TestFromSetupPy(t *testing.T) {
	reqs, err := pip.FromSetupPy("testdata/setup.py")
	assert.Nil(t, err)
	assert.Contains(t, reqs, pip.Requirement{Name: "simple", Constraints: []pip.Constraint{{Revision: "1.0.0", Operator: "=="}}})
	assert.Contains(t, reqs, pip.Requirement{Name: "gteq", Constraints: []pip.Constraint{{Revision: "2.0.0", Operator: ">="}}})
	assert.Contains(t, reqs, pip.Requirement{Name: "sameline", Constraints: []pip.Constraint{{Revision: "1.0.0", Operator: "=="}}})
	assert.Contains(t, reqs, pip.Requirement{Name: "latest", Constraints: nil})
	assert.Contains(t, reqs, pip.Requirement{Name: "with-double-quotes", Constraints: nil})
}

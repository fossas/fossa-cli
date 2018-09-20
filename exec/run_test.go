package exec_test

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/exec"
)

func TestSetEnv(t *testing.T) {
	c, _ := exec.BuildExec(exec.Cmd{
		Name: "example",
		Env: map[string]string{
			"foo": "bar",
		},
	})

	assert.Equal(t, []string{"foo=bar"}, c.Env)
	assert.Len(t, c.Env, 1)
}

func TestAppendEnv(t *testing.T) {
	os.Setenv("alice", "bob")
	c, _ := exec.BuildExec(exec.Cmd{
		Name: "example",
		WithEnv: map[string]string{
			"foo": "bar",
		},
	})

	assert.Contains(t, c.Env, "foo=bar")
	assert.Contains(t, c.Env, "alice=bob")
}

func TestDefaultEnv(t *testing.T) {
	os.Setenv("alice", "bob")
	c, _ := exec.BuildExec(exec.Cmd{
		Name: "example",
	})
	assert.Contains(t, c.Env, "alice=bob")
}

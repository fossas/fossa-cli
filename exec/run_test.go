package exec_test

import (
	"os"
	"testing"
	"time"

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

func TestRun(t *testing.T) {
	command := exec.Cmd{
		Name: "pwd",
	}
	stdout, _, err := exec.Run(command)
	assert.NoError(t, err)
	assert.NotEmpty(t, stdout)
}

func TestRunTimeout(t *testing.T) {
	command := exec.Cmd{
		Name:    "sleep",
		Argv:    []string{"2"},
		Timeout: 3 * time.Second,
	}
	_, _, err := exec.Run(command)
	assert.NoError(t, err)

	command.Timeout = 1 * time.Second
	_, _, err = exec.Run(command)
	assert.Contains(t, err.Error(), "timed out")
}

func TestRunRetry(t *testing.T) {
	command := exec.Cmd{
		Name:    "sleep",
		Argv:    []string{"3"},
		Timeout: 1 * time.Second,
		Retries: 1,
	}

	start := time.Now()
	_, _, err := exec.Run(command)
	assert.Error(t, err)
	assert.WithinDuration(t, start, time.Now(), 3*time.Second)

	command.Retries = 4
	start = time.Now()
	_, _, err = exec.Run(command)
	assert.Error(t, err)
	// 4 retries with a 1 second timeout means that the command should take 5 seconds to error.
	assert.True(t, time.Since(start) > 5*time.Second)
	assert.True(t, time.Since(start) < 6*time.Second)
}

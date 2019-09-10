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

	stdout, stderr, err := exec.Run(command)
	assert.NoError(t, err)
	assert.NotEmpty(t, stdout)
	assert.Empty(t, stderr)
}

func TestRunFails(t *testing.T) {
	command := exec.Cmd{
		Name: "FakeCommand",
	}

	stdout, stderr, err := exec.Run(command)
	assert.Error(t, err)
	assert.Empty(t, stdout)
	assert.Empty(t, stderr)
}

func TestRunTimeoutSucceeds(t *testing.T) {
	command := exec.Cmd{
		Name:    "pwd",
		Timeout: "3s",
	}

	stdout, stderr, err := exec.Run(command)
	assert.NoError(t, err)
	assert.NotEmpty(t, stdout)
	assert.Empty(t, stderr)
}

func TestRunTimeoutTimesOut(t *testing.T) {
	command := exec.Cmd{
		Name:    "sleep",
		Argv:    []string{"2"},
		Timeout: "3s",
	}
	_, _, err := exec.Run(command)
	assert.NoError(t, err)

	command.Timeout = "1s"
	_, _, err = exec.Run(command)
	assert.Contains(t, err.Error(), "timed out")
}

func TestRunRetry(t *testing.T) {
	command := exec.Cmd{
		Name:    "sleep",
		Argv:    []string{"4"},
		Timeout: "1s",
		Retries: 1,
	}

	start := time.Now()
	_, _, err := exec.Run(command)
	assert.Error(t, err)
	assert.WithinDuration(t, start, time.Now(), 3*time.Second)

	command.Retries = 5
	start = time.Now()
	_, _, err = exec.Run(command)
	end := time.Now()
	assert.Error(t, err)
	// 5 retries with a 1 second timeout means the command should take more than 6 seconds,
	// opposed to the 4 seconds it would take without the timeout.
	assert.True(t, end.Sub(start) > 6*time.Second)
}

func TestRunCaptureStderr(t *testing.T) {
	command := exec.Cmd{
		Name: "pwd",
		Argv: []string{"--invalid-flag"},
	}
	stdout, stderr, err := exec.Run(command)
	assert.Error(t, err)
	assert.Empty(t, stdout)
	assert.NotEmpty(t, stderr)
}

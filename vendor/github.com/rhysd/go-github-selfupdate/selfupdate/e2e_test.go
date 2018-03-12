package selfupdate

import (
	"os"
	"os/exec"
	"testing"
)

func TestRunSelfUpdateExample(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode.")
	}

	t.Skip("TODO")

	if err := exec.Command("go", "build", "../cmd/selfupdate-example").Run(); err != nil {
		t.Fatal(err)
	}
	defer os.Remove("selfupdate-example")

	// TODO
}

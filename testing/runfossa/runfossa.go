package runfossa

import (
	"bytes"
	"io"
	"os"

	"github.com/fossas/fossa-cli/cmd/fossa/app"
	"github.com/fossas/fossa-cli/exec"
)

// Init executes fossa init the provided directory
func Init(projectDir string) error {
	cmd := exec.Cmd{
		Argv:    []string{"init"},
		Name:    "fossa",
		Dir:     projectDir,
		Command: "fossa",
	}

	_, errMsg, err := exec.Run(cmd)
	if err != nil {
		println(errMsg)
	}

	return err
}

func ReportLicenses(dir string, params ...string) (string, error) {
	old := os.Stdout
	r, w, _ := os.Pipe()
	os.Stdout = w

	outC := make(chan string)

	origApiKey := os.Getenv("FOSSA_API_KEY")
	// os.Setenv("FOSSA_API_KEY", "abc")
	os.Chdir(dir)
	cliCommand := []string{"fossa", "report", "licenses"}

	cliCommand = append(cliCommand, params...)

	go func() {
		var buf bytes.Buffer
		io.Copy(&buf, r)
		outC <- buf.String()
	}()

	err := app.New().Run(cliCommand)
	os.Setenv("FOSSA_API_KEY", origApiKey)

	// back to normal state
	w.Close()
	os.Stdout = old // restoring the real stdout
	out := <-outC

	return out, err
}

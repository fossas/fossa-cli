package pip

import (
	"encoding/json"
	"io/ioutil"
	"os"

	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/buildtools/pip/bindata"
	"github.com/fossas/fossa-cli/exec"
)

//go:generate go-bindata -pkg bindata -modtime 1 -o ./bindata/bindata.go ./bindata/pipdeptree.py

type DepTree struct {
	Package      string `json:"package_name"`
	Resolved     string `json:"installed_version"`
	Target       string `json:"required_version"`
	Dependencies []DepTree
}

func (p *Pip) DepTree() ([]DepTree, error) {
	// Write helper to disk.
	src, err := bindata.Asset("bindata/pipdeptree.py")
	if err != nil {
		return nil, errors.Wrap(err, "could not read `pipdeptree` helper")
	}

	pipdeptreeFile, err := ioutil.TempFile("", "fossa-cli-pipdeptree-")
	if err != nil {
		return nil, errors.Wrap(err, "could not create temp file to write `pipdeptree` helper")
	}
	defer pipdeptreeFile.Close()
	defer os.Remove(pipdeptreeFile.Name())

	n, err := pipdeptreeFile.Write(src)
	if len(src) != n || err != nil {
		return nil, errors.Wrap(err, "could not write `pipdeptree` helper")
	}
	err = pipdeptreeFile.Sync()
	if err != nil {
		return nil, errors.Wrap(err, "could not flush `pipdeptree` helper to storage")
	}

	// Run helper.
	out, _, err := exec.Run(exec.Cmd{
		Name: p.PythonCmd,
		Argv: []string{pipdeptreeFile.Name(), "--local-only", "--json-tree"},
	})
	if err != nil {
		return nil, errors.Wrap(err, "could not run `pipdeptree`")
	}

	// Parse output.
	var parsed []DepTree
	err = json.Unmarshal([]byte(out), &parsed)
	if err != nil {
		return nil, errors.Wrap(err, "could not parse `pipdeptree` output")
	}

	return parsed, nil
}

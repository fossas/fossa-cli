package debian

import (
	"fmt"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
)

type Cmd struct {
	NoUpload bool
	DebCmd   func(string) (string, error)
}

func New() Cmd {
	return Cmd{
		NoUpload: false,
		DebCmd:   DebCommand,
	}
}

func (d Cmd) Dependencies(target string) (graph.Deps, error) {
	fmt.Println(target)
	shellOut, err := DebCommand(target)
	if err != nil {
		return graph.Deps{}, err
	}

	fmt.Println(shellOut)
	return graph.Deps{}, errors.New("Not implemented")
}

func DebCommand(target string) (string, error) {
	out, _, err := exec.Run(exec.Cmd{
		Name: "apt-cache",
		Argv: []string{"depends", target},
	})
	fmt.Println(out, err)
	if err != nil {
		return "", nil
	}
	return out, nil
}

func ParseDeps(output string) (string, error) {
	return "", nil
}

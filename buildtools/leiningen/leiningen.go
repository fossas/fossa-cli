package leiningen

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
)

type Input interface {
	DependencyGraph(string) (graph.Deps, *errors.Error)
}

type Shell struct {
	Binary string
	Dir    string
	Cmd    func(string, ...string) (string, error)
}

func NewInput(binary, dir string) Input {
	return Shell{
		Binary: binary,
		Dir:    dir,
		Cmd:    Cmd,
	}
}
func Cmd(command string, args ...string) (string, error) {
	cmd := exec.Cmd{
		Name: command,
		Argv: args,
	}

	stdout, stderr, err := exec.Run(cmd)
	if stderr != "" {
		return stdout, errors.Errorf("%s", stderr)
	}

	return stdout, err
}

func ValidBinary(dir string) (string, error) {
	lein, _, err := exec.Which("--version", os.Getenv("FOSSA_LEIN_CMD"), "lein")
	return lein, err
}

func (s Shell) DependencyGraph(target string) (graph.Deps, *errors.Error) {
	fmt.Println("Dep graph")
	out, err := s.Cmd(s.Binary, "deps", ":tree")
	fmt.Println(out, err)
	return graph.Deps{}, nil
}

func ProjectFile(dir, file string) (graph.Deps, error) {
	fmt.Println(file)
	project, err := files.Read(filepath.Join(dir, file))
	fmt.Println(string(project), err)

	return graph.Deps{}, nil
}

package bundler

import (
	"strings"

	"github.com/fossas/fossa-cli/exec"
	"github.com/apex/log"
)

type Bundler struct {
	Cmd string
}

type Gem struct {
	Name     string
	Revision string
}

func (b *Bundler) List() ([]Gem, error) {
	stdout, _, err := exec.Run(exec.Cmd{
		Name: b.Cmd,
		Argv: []string{"list"},
	})
	if err != nil {
		return nil, err
	}
	contents := strings.TrimSpace(strings.TrimPrefix(stdout, "Gems included by the bundle:\n"))
	lines := strings.Split(contents, "\n")
	var gems []Gem
	for _, line := range lines[1:] { // The first line is "Gems included by the bundle:"
		trimmed := strings.TrimPrefix(line, "  * ")
		log.Logger.Debugf("trimmed: %#v", trimmed)
		sections := strings.Split(trimmed, " ")
		gems = append(gems, Gem{
			Name:     sections[0],
			Revision: sections[1],
		})
	}
	return gems, nil
}

func (b *Bundler) Install(flags ...string) error {
	if flags == nil {
		flags = []string{"--frozen", "--deployment"}
	}
	_, _, err := exec.Run(exec.Cmd{
		Name: b.Cmd,
		Argv: append([]string{"install"}, flags...),
	})
	return err
}

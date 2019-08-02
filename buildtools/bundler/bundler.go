package bundler

import (
	"fmt"
	"strings"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
)

type Bundler struct {
	Cmd string
}

type Gem struct {
	Name     string
	Revision string
}

func (b *Bundler) List() ([]Gem, *errors.Error) {
	stdout, stderr, err := exec.Run(exec.Cmd{
		Name: b.Cmd,
		Argv: []string{"list"},
	})
	if err != nil {
		return nil, &errors.Error{
			Cause:           err,
			Type:            errors.Exec,
			Troubleshooting: fmt.Sprintf("Fossa could not run `%s list` within the current directory. Try running this command and ensure that %s is installed in your environment.\nstdout: %s\nstderr: %s", b.Cmd, b.Cmd, stdout, stderr),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/ruby.md#support",
		}
	}
	contents := strings.TrimSpace(strings.TrimPrefix(stdout, "Gems included by the bundle:\n"))
	lines := strings.Split(contents, "\n")
	var gems []Gem
	for _, line := range lines[1:] { // The first line is "Gems included by the bundle:"
		trimmed := strings.TrimPrefix(line, "  * ")
		log.WithField("trimmed", trimmed).Debug("parsing line")
		sections := strings.Split(trimmed, " ")
		gems = append(gems, Gem{
			Name:     sections[0],
			Revision: sections[1],
		})
	}
	return gems, nil
}

func (b *Bundler) Install(flags ...string) *errors.Error {
	if flags == nil {
		flags = []string{"--frozen", "--deployment"}
	}
	stdout, stderr, err := exec.Run(exec.Cmd{
		Name: b.Cmd,
		Argv: append([]string{"install"}, flags...),
	})
	if err != nil {
		return &errors.Error{
			Cause:           err,
			Type:            errors.Exec,
			Troubleshooting: fmt.Sprintf("Fossa could not run `%s install --frozen --deployment` within the current directory. Try running this command and ensure that %s is installed in your environment.\nstdout: %s\nstderr: %s", b.Cmd, b.Cmd, stdout, stderr),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/ruby.md#support",
		}

	}
	return nil
}

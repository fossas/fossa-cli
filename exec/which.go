package exec

import (
	"errors"

	"github.com/apex/log"
)

// Which picks a command out of a list of candidates.
func Which(arg string, cmds ...string) (cmd string, output string, err error) {
	return WhichArgs([]string{arg}, cmds...)
}

// WhichArgs is `Which` but passes multiple arguments to each candidate.
func WhichArgs(argv []string, cmds ...string) (cmd string, output string, err error) {
	return WhichWithResolver(cmds, func(cmd string) (string, bool, error) {
		stdout, stderr, err := Run(Cmd{
			Name: cmd,
			Argv: argv,
		})
		if err != nil {
			return "", false, err
		}
		if stdout == "" {
			return stderr, true, nil
		}
		return stdout, true, nil
	})
}

// A WhichResolver takes a candidate command and returns whether to choose it.
type WhichResolver func(cmd string) (output string, ok bool, err error)

// WhichWithResolver is `Which` with a custom resolution strategy.
func WhichWithResolver(cmds []string, resolve WhichResolver) (string, string, error) {
	for _, cmd := range cmds {
		version, ok, err := resolve(cmd)
		if ok {
			return cmd, version, nil
		}
		log.WithError(err).WithFields(log.Fields{
			"cmd":     cmd,
			"version": version,
		})
	}
	return "", "", errors.New("could not resolve command")
}

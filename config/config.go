// Package config implements application-level configuration functionality.
package config

import (
	"os"

	isatty "github.com/mattn/go-isatty"
	"github.com/urfave/cli"
	git "gopkg.in/src-d/go-git.v4"

	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/files"
)

var (
	ctx  *cli.Context
	file configFileV1
	repo *git.Repository
)

func Init(c *cli.Context) error {
	ctx = c
	err := readFile(c)
	if err != nil {
		return err
	}
	r, err := git.PlainOpen(".")
	if err == git.ErrRepositoryNotExists {
		return nil
	}
	if err != nil {
		return err
	}
	repo = r
	return nil
}

func readFile(c *cli.Context) error {
	filepath, err := TryFiles(ctx.String(flags.ConfigFlagName), ".fossa.yml", ".fossa.yaml")
	if err == ErrFileNotFound {
		return nil
	}
	if err != nil {
		return err
	}
	return files.ReadYAML(&file, filepath)
}

func Interactive() bool {
	return isatty.IsTerminal(os.Stdout.Fd()) && !ctx.Bool(flags.NoAnsiFlagName)
}

func Debug() bool {
	return ctx.Bool(flags.DebugFlagName)
}

func Endpoint() string {
	return TryStrings(ctx.String(flags.EndpointFlagName), file.CLI.Server)
}

func APIKey() string {
	return TryStrings(file.CLI.APIKey, os.Getenv("FOSSA_API_KEY"))
}

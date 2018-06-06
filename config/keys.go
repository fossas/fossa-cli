package config

import (
	"os"

	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/module"
	isatty "github.com/mattn/go-isatty"
)

/**** Global configuration keys ****/

var (
	filename string
)

// Interactive is true if the user desires interactive output.
func Interactive() bool {
	return isatty.IsTerminal(os.Stdout.Fd()) && !ctx.Bool(flags.NoAnsi)
}

// Debug is true if the user has requested debug-level logging.
func Debug() bool {
	return ctx.Bool(flags.Debug)
}

// Filepath is the configuration file path.
func Filepath() string {
	return filename
}

/**** API configuration keys ****/

// APIKey is user's FOSSA API key.
func APIKey() string {
	return TryStrings(file.APIKey(), os.Getenv("FOSSA_API_KEY"))
}

// Endpoint is the desired FOSSA backend endpoint.
func Endpoint() string {
	return TryStrings(ctx.String(flags.Endpoint), file.Server())
}

/**** Project configuration keys ****/

func Fetcher() string {
	return TryStrings(ctx.String(flags.Fetcher), file.Fetcher())
}

// TODO: auto-detect based on VCS
func Project() string {
	return TryStrings(ctx.String(flags.Project), file.Project())
}

// TODO: auto-detect based on VCS
func Branch() string {
	return TryStrings(ctx.String(flags.Branch), file.Branch())
}

/**** Analysis configuration keys ****/

func Modules() []module.Module {
	args := ctx.Args()
	if args.Present() {
		if ctx.NArg() != 1 {
		}
		options := ctx.StringSlice(flags.Option)
		for _, a := range args {
			sections := strings.Split(a, ":")
			modules =
			mtype := sections[0]
		}
	}
	return file.Modules()
}

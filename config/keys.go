package config

import (
	"errors"
	"os"
	"strconv"
	"strings"

	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
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

func Title() string {
	return TryStrings(ctx.String(flags.Title), file.Title())
}

func Fetcher() string {
	return TryStrings(ctx.String(flags.Fetcher), file.Fetcher())
}

// TODO: auto-detect based on VCS
func Project() string {
	return TryStrings(ctx.String(flags.Project), file.Project())
}

// TODO: auto-detect based on VCS
func Revision() string {
	return TryStrings(ctx.String(flags.Revision), file.Revision())
}

// TODO: auto-detect based on VCS
func Branch() string {
	return TryStrings(ctx.String(flags.Branch), file.Branch())
}

/**** Analysis configuration keys ****/

func Modules() ([]module.Module, error) {
	// If arguments are present, use arguments.
	args := ctx.Args()
	if args.Present() {
		// Validate arguments.
		if ctx.NArg() != 1 {
			return nil, errors.New("must specify exactly 1 module")
		}

		// Parse module.
		arg := args.First()
		sections := strings.Split(arg, ":")
		typename := sections[0]
		name := strings.Join(sections[1:], "")
		mtype, err := pkg.ParseType(typename)
		if err != nil {
			return nil, err
		}

		// Parse options.
		optionFs := ctx.StringSlice(flags.Option)
		options := make(map[string]interface{})
		for _, option := range optionFs {
			sections := strings.Split(option, ":")
			key := sections[0]
			value := strings.Join(sections[1:], "")
			// Attempt to parse as boolean.
			if value == "true" {
				options[key] = true
				continue
			} else if value == "false" {
				options[key] = false
				continue
			} else if i, err := strconv.Atoi(value); err == nil {
				// Attempt to parse as number.
				options[key] = i
			} else if strings.HasPrefix(value, "[") && strings.HasSuffix(value, "]") {
				// Attempt to parse as list.
				noPrefix := strings.TrimPrefix(value, "[")
				noSuffix := strings.TrimPrefix(noPrefix, "]")
				noSpace := strings.TrimSpace(noSuffix)
				elements := strings.Split(noSpace, ",")
				options[key] = elements
			} else {
				// Treat as a string.
				options[key] = value
			}
		}

		m := []module.Module{module.Module{
			Name:        name,
			Type:        mtype,
			BuildTarget: name,
			Options:     options,
		}}
		log.Logger.Debugf("Parsed module from arguments: %#v", m)
		return m, nil
	}

	return file.Modules(), nil
}

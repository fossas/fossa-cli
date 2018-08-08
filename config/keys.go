package config

import (
	"errors"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/apex/log"
	isatty "github.com/mattn/go-isatty"

	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

/**** Mock configuration values ****/
var (
	MockBranch string
)

/**** Global configuration keys ****/

var (
	filename string
)

// Interactive is true if the user desires interactive output.
func Interactive() bool {
	return isatty.IsTerminal(os.Stderr.Fd()) && !BoolFlag(flags.NoAnsi)
}

// Debug is true if the user has requested debug-level logging.
func Debug() bool {
	return BoolFlag(flags.Debug)
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
	return TryStrings(StringFlag(flags.Endpoint), file.Server(), "https://app.fossa.io")
}

/**** Project configuration keys ****/

func Title() string {
	dir, _ := os.Getwd()
	return TryStrings(StringFlag(flags.Title), file.Title(), filepath.Base(dir))
}

func Fetcher() string {
	return TryStrings(StringFlag(flags.Fetcher), file.Fetcher(), "custom")
}

func Project() string {
	inferred := ""
	if repo != nil {
		origin, err := repo.Remote("origin")
		if err == nil && origin != nil {
			inferred = origin.Config().URLs[0]
		}
	}
	return TryStrings(StringFlag(flags.Project), file.Project(), inferred)
}

func Revision() string {
	inferred := ""
	if repo != nil {
		revision, err := repo.Head()
		if err == nil {
			inferred = revision.Hash().String()
		}
	}
	return TryStrings(StringFlag(flags.Revision), file.Revision(), inferred)
}

func Branch() string {
	inferred := ""
	if repo != nil {
		revision, err := repo.Head()
		if err == nil {
			// TODO: check whether this prefix trimming is actually correct.
			inferred = strings.TrimPrefix(revision.Name().String(), "refs/heads/")
		}
	}
	return TryStrings(MockBranch, StringFlag(flags.Branch), file.Branch(), inferred, "master")
}

/**** Analysis configuration keys ****/

func Options() (map[string]interface{}, error) {
	opts := ctx.StringSlice(flags.Option)
	opts = append(opts, ctx.GlobalStringSlice(flags.Option)...)

	options := make(map[string]interface{})
	for _, option := range opts {
		sections := strings.Split(option, ":")
		key := sections[0]
		value := strings.Join(sections[1:], ":")
		log.WithFields(log.Fields{
			"sections": sections,
			"key":      key,
			"value":    value,
		}).Debug("parsing options")
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
		} else {
			// Treat as a string.
			options[key] = value
		}
	}

	return options, nil
}

func Modules() ([]module.Module, error) {
	args := ctx.Args()
	options, err := Options()
	if err != nil {
		return nil, err
	}

	log.WithFields(log.Fields{
		"args":    args,
		"options": options,
	}).Debug("parsing modules")

	// If arguments are present, prefer arguments over the configuration file.
	if args.Present() {
		// Validate arguments.
		if ctx.NArg() != 1 {
			return nil, errors.New("must specify exactly 1 module")
		}

		// Parse module.
		arg := args.First()
		sections := strings.Split(arg, ":")
		typename := sections[0]
		name := strings.Join(sections[1:], ":")
		mtype, err := pkg.ParseType(typename)
		if err != nil {
			return nil, err
		}

		m := []module.Module{module.Module{
			Name:        name,
			Type:        mtype,
			BuildTarget: name,
			Options:     options,
		}}

		log.WithField("module", m).Debug("parsed module")
		return m, nil
	}

	if l := len(options); l > 0 {
		log.
			WithFields(log.Fields{
				"options": options,
			}).
			Warnf(
				"Found %d options via flag, but modules are being loaded from configuration file. Ignoring options.",
				len(options),
			)
	}

	// TODO: specifying zero modules should be an error (we should add a test for this)
	return file.Modules(), nil
}

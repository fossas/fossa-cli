package config

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/apex/log"
	"github.com/mattn/go-isatty"

	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/errors"
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

// Version outputs the installed FOSSA CLI version.
func Version() int {
	return file.GetVersion()
}

// Insecure is true if the user wants to skip TLS Certificate authenticity checks
func Insecure() bool {
	return BoolFlag(flags.Insecure)
}

// Interactive is true if the user desires interactive output.
func Interactive() bool {
	return isatty.IsTerminal(os.Stderr.Fd()) && !BoolFlag(flags.NoAnsi)
}

// Debug is true if the user has requested debug-level logging.
func Debug() bool {
	return BoolFlag(flags.Debug)
}

// Verbose is true if the user has requested verbose output.
func Verbose() bool {
	return BoolFlag(flags.Verbose)
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

const DefaultEndpoint = "https://app.fossa.com"

// Endpoint is the desired FOSSA backend endpoint.
func Endpoint() string {
	return TryStrings(StringFlag(flags.Endpoint), file.Server(), DefaultEndpoint)
}

/**** Project configuration keys ****/

func Title() string {
	dir, _ := os.Getwd()
	return TryStrings(StringFlag(flags.Title), file.Title(), Project(), filepath.Base(dir))
}

func Fetcher() string {
	return TryStrings(StringFlag(flags.Fetcher), file.Fetcher(), "custom")
}

func Project() string {
	inferred := ""
	if repo != nil {
		inferred = repo.Project()
	}
	return TryStrings(StringFlag(flags.Project), file.Project(), inferred)
}

func Revision() string {
	inferred := ""
	if repo != nil {
		inferred = repo.Head().RevisionID
	}
	return TryStrings(StringFlag(flags.Revision), file.Revision(), inferred)
}

func Branch() string {
	inferred := ""
	if repo != nil {
		inferred = repo.Head().Branch
	}
	return TryStrings(MockBranch, StringFlag(flags.Branch), file.Branch(), inferred, "master")
}

func ProjectURL() string {
	return TryStrings(StringFlag(flags.ProjectURL), file.ProjectURL(), "")
}

func JIRAProjectKey() string {
	return TryStrings(StringFlag(flags.JIRAProjectKey), file.JIRAProjectKey(), "")
}

func Link() string {
	return TryStrings(StringFlag(flags.Link), file.Link(), "")
}

func Team() string {
	return TryStrings(StringFlag(flags.Team), file.Team(), "")
}

func Policy() string {
	return TryStrings(StringFlag(flags.Policy), file.Policy(), "")
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
	log.WithFields(log.Fields{"args": args}).Debug("parsing modules")

	var modules []module.Module

	// If arguments are present, prefer arguments over the configuration file.
	if args.Present() {
		// Validate arguments.
		if ctx.NArg() != 1 {
			return nil, &errors.Error{
				Cause:           errors.New(fmt.Sprintf("must specify exactly 1 module in arguments: %s", args)),
				Troubleshooting: fmt.Sprintf("Ensure that you are specifying only one module in the arguments, currently specified `%s`", args),
				Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/user-guide.md/#argument-configuration",
				Message:         errors.ArgumentModuleMessage,
			}
		}

		// Parse module.
		arg := args.First()
		sections := strings.Split(arg, ":")
		typename := sections[0]
		name := strings.Join(sections[1:], ":")
		mtype, err := pkg.ParseType(typename)
		if err != nil {
			return nil, &errors.Error{
				Cause:           err,
				Troubleshooting: fmt.Sprintf("Ensure that you are specifying a valid package type. Arguments supplied `%s`", args),
				Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/user-guide.md/#argument-configuration",
				Message:         errors.ArgumentModuleMessage,
			}
		}

		// Note that these parsed modules do not have any options set yet.
		m := module.Module{
			Name:        name,
			Type:        mtype,
			BuildTarget: name,
		}

		log.WithField("module", m).Debug("parsed module")
		modules = append(modules, m)
	} else {
		// Otherwise, get the modules from the configuration file.
		modules = file.Modules()
		if len(modules) == 0 {
			return modules, errors.New("No modules provided")
		}
	}

	// Parse options set via the command line.
	options, err := Options()
	if err != nil {
		return nil, err
	}
	log.WithFields(log.Fields{"options": options}).Debug("parsing options")

	// Set options passed via the command line on all modules. For modules where
	// this conflicts with a configuration file option, prefer the command line
	// option.
	for i, m := range modules {
		if m.Options == nil {
			modules[i].Options = make(map[string]interface{})
		}

		// We iterate over and copy the command-line options instead of setting the
		// module options equal to avoid _unsetting_ module options that were set
		// within the configuration file.
		for key, val := range options {
			modules[i].Options[key] = val
		}
	}

	return modules, nil
}

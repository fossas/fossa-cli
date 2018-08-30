package bundler

import (
	"regexp"
	"strings"

	"github.com/apex/log"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/files"
)

type Lockfile struct {
	Git          []Section
	Path         []Section
	Gem          []Section
	Dependencies []Requirement
}

type Section struct {
	Type     string
	Remote   string
	Revision string
	Ref      string
	Tag      string
	Branch   string
	Specs    []Spec
}

type Spec struct {
	Name         string
	Version      string
	Dependencies []Requirement
}

type Requirement struct {
	Name    string
	Pinned  bool
	Version VersionSpecifier
}

func (r *Requirement) String() string {
	s := r.Name
	if r.Pinned {
		s += "!"
	}
	if r.Version != "" {
		s += " (" + string(r.Version) + ")"
	}
	return s
}

// ^(leading whitespace)(name)(optional: exclamation mark or (space + (version specifier) within parentheses (optional: exclamation mark)))$
var requirementsRegex = regexp.MustCompile("^( *?)(\\S+?)(?:\\!?|( \\((.*?)\\)\\!?)?)$")

// TODO: actually parse these. We ignore them right now, so I haven't bothered
// implementing parsing logic.
type VersionSpecifier string

func FromLockfile(filename string) (Lockfile, error) {
	contents, err := files.Read(filename)
	if err != nil {
		return Lockfile{}, errors.Wrap(err, "could not read Gemfile.lock")
	}

	var lockfile Lockfile
	sections := strings.Split(string(contents), "\n\n")
	for _, section := range sections {
		lines := strings.Split(strings.TrimSpace(section), "\n")
		header := lines[0]

		switch header {
		case "GIT":
			lockfile.Git = append(lockfile.Git, ParseSpecSection(section))
		case "PATH":
			lockfile.Path = append(lockfile.Path, ParseSpecSection(section))
		case "GEM":
			lockfile.Gem = append(lockfile.Gem, ParseSpecSection(section))
		case "DEPENDENCIES":
			lockfile.Dependencies = ParseDependenciesSection(section)
		default:
			continue
		}
	}

	return lockfile, nil
}

func ParseSpecSection(section string) Section {
	lines := strings.Split(strings.TrimSpace(section), "\n")
	header := lines[0]

	remoteRegex := regexp.MustCompile("\n  remote: (.*?)\n")
	revisionRegex := regexp.MustCompile("\n  revision: (.*?)\n")
	refRegex := regexp.MustCompile("\n  ref: (.*?)\n")
	tagRegex := regexp.MustCompile("\n  tag: (.*?)\n")
	branchRegex := regexp.MustCompile("\n  branch: (.*?)\n")

	specs := regexp.MustCompile("(?s)\n  specs:\n(.*?)$")

	return Section{
		Type:     header,
		Remote:   fromMaybe(remoteRegex.FindStringSubmatch(section), 1),
		Revision: fromMaybe(revisionRegex.FindStringSubmatch(section), 1),
		Ref:      fromMaybe(refRegex.FindStringSubmatch(section), 1),
		Tag:      fromMaybe(tagRegex.FindStringSubmatch(section), 1),
		Branch:   fromMaybe(branchRegex.FindStringSubmatch(section), 1),
		Specs:    ParseSpecs(specs.FindStringSubmatch(section)[1]),
	}
}

func fromMaybe(s []string, i int) string {
	if len(s) > i {
		return s[i]
	}
	return ""
}

func ParseSpecs(s string) []Spec {
	lines := strings.Split(s, "\n")
	var specs []Spec
	var curr Spec
	for i, line := range lines {
		matches := requirementsRegex.FindStringSubmatch(line)

		spaces := matches[1]
		name := matches[2]
		version := ""
		if len(matches) == 5 {
			version = matches[4]
		}
		log.WithFields(log.Fields{
			"spaces":  spaces,
			"name":    name,
			"version": version,
		}).Debug("parsing spec line")

		switch len(spaces) {
		// This is a spec.
		case 4:
			// Push the previous spec. Don't push the initial spec (which is zero).
			if i != 0 {
				specs = append(specs, curr)
			}
			curr = Spec{
				Name:         name,
				Version:      version,
				Dependencies: []Requirement{},
			}
		// This is a requirement.
		case 6:
			curr.Dependencies = append(curr.Dependencies, Requirement{
				Name:    name,
				Version: VersionSpecifier(version),
				Pinned:  strings.HasSuffix(name, "!"),
			})
		default:
			// TODO: this should return an error instead of panicking.
			log.Fatal("Malformed lockfile")
		}
	}
	// Push the last spec.
	log.WithField("spec", curr).Debug("push last spec")
	specs = append(specs, curr)

	return specs
}

func ParseDependenciesSection(section string) []Requirement {
	lines := strings.Split(strings.TrimSpace(section), "\n")

	// Ignore header.
	var requirements []Requirement
	for _, line := range lines[1:] {
		matches := requirementsRegex.FindStringSubmatch(line)
		spaces := matches[1]
		name := matches[2]
		version := ""
		if len(matches) == 5 {
			version = matches[4]
		}
		log.WithFields(log.Fields{
			"spaces":  spaces,
			"name":    name,
			"version": version,
		}).Debug("parsing dependency line")
		requirements = append(requirements, Requirement{
			Name:    strings.TrimSuffix(name, "!"),
			Pinned:  strings.HasSuffix(name, "!"),
			Version: VersionSpecifier(version),
		})
	}

	return requirements
}

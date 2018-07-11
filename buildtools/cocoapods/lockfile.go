package cocoapods

import (
	"path/filepath"
	"regexp"
	"strings"

	"github.com/fossas/fossa-cli/files"
)

type RawLockfile struct {
	Pods            []interface{}                `yaml:"PODS"`         // Transitive dependencies (this is actually `[](map[string][]string | string)`)
	Dependencies    []string                     `yaml:"DEPENDENCIES"` // Direct dependencies
	CheckoutOptions map[string]map[string]string `yaml:"CHECKOUT OPTIONS"`
	ExternalSources map[string]map[string]string `yaml:"EXTERNAL SOURCES"`
	SpecRepos       map[string][]string          `yaml:"SPEC REPOS"`
}

type Lockfile struct {
	Pods            []Spec
	Dependencies    []Requirement
	CheckoutOptions map[string]CheckoutOptions
	ExternalSources map[string]ExternalSource
	SpecRepos       map[string][]string
}

type Spec struct {
	Name         string
	Version      string
	Dependencies []Requirement
}

type Requirement struct {
	Name       string
	Version    string
	Repository string
	Branch     string
	Tag        string

	Original string
}

func (r *Requirement) String() string {
	return r.Original
}

type CheckoutOptions struct {
	Git    string
	Commit string
	Tag    string
}

type ExternalSource struct {
	Git    string
	Branch string
	Tag    string

	Path string
}

func FromLockfile(filename ...string) (Lockfile, error) {
	var raw RawLockfile
	err := files.ReadYAML(&raw, filepath.Join(filename...))

	var lockfile Lockfile
	// Parse pods.
	for _, rawPod := range raw.Pods {
		switch r := rawPod.(type) {
		case map[interface{}]interface{}:
			for pod, deps := range r {
				var reqs []Requirement
				dep := ParseRequirement(pod.(string))
				for _, req := range deps.([]interface{}) {
					reqs = append(reqs, ParseRequirement(req.(string)))
				}
				lockfile.Pods = append(lockfile.Pods, Spec{
					Name:         dep.Name,
					Version:      dep.Version,
					Dependencies: reqs,
				})
			}
		case string:
			dep := ParseRequirement(r)
			lockfile.Pods = append(lockfile.Pods, Spec{
				Name:    dep.Name,
				Version: dep.Version,
			})
		default:
			panic(rawPod)
		}
	}

	// Parse dependencies.
	for _, dep := range raw.Dependencies {
		lockfile.Dependencies = append(lockfile.Dependencies, ParseRequirement(dep))
	}

	// Parse checkout options.
	lockfile.CheckoutOptions = make(map[string]CheckoutOptions)
	for dep, options := range raw.CheckoutOptions {
		lockfile.CheckoutOptions[dep] = CheckoutOptions{
			Git:    options[":git"],
			Commit: options[":commit"],
			Tag:    options[":tag"],
		}
	}

	// Parse external sources.
	lockfile.ExternalSources = make(map[string]ExternalSource)
	for dep, options := range raw.ExternalSources {
		lockfile.ExternalSources[dep] = ExternalSource{
			Git:    options[":git"],
			Branch: options[":branch"],
			Tag:    options[":tag"],
			Path:   options[":path"],
		}
	}

	// Parse spec repos.
	lockfile.SpecRepos = raw.SpecRepos

	return lockfile, err
}

func ParseRequirement(req string) Requirement {
	splits := strings.Split(req, " ")
	name := splits[0]
	versionSpecifier := strings.TrimPrefix(strings.TrimSuffix(strings.Join(splits[1:], " "), ")"), "(")

	if strings.HasPrefix(versionSpecifier, "from ") {
		repoRegexp := regexp.MustCompile("from `(.*?)`")
		branchRegexp := regexp.MustCompile("branch `(.*?)`")
		tagRegexp := regexp.MustCompile("tag `(.*?)`")

		repoMatches := repoRegexp.FindStringSubmatch(versionSpecifier)
		repo := ""
		if len(repoMatches) == 2 {
			repo = repoMatches[1]
		}
		branchMatches := branchRegexp.FindStringSubmatch(versionSpecifier)
		branch := ""
		if len(branchMatches) == 2 {
			branch = branchMatches[1]
		}
		tagMatches := tagRegexp.FindStringSubmatch(versionSpecifier)
		tag := ""
		if len(tagMatches) == 2 {
			tag = tagMatches[1]
		}

		return Requirement{
			Name:       name,
			Repository: repo,
			Branch:     branch,
			Tag:        tag,

			Original: req,
		}
	}
	return Requirement{
		Name:    name,
		Version: versionSpecifier,

		Original: req,
	}
}

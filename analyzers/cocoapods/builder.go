package cocoapods

import (
	"fmt"
	"io/ioutil"
	"path/filepath"
	"regexp"
	"strings"

	yaml "gopkg.in/yaml.v2"

	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

const podNameRegex = `[\w/\.\-\\+]+`

// CocoapodsBuilder implements Builder for Cocoapods (podfile & podfile.lock) builds
type CocoapodsBuilder struct {
	CocoapodsCmd     string
	CocoapodsVersion string
}

// CocoapodsModule implements Dependency for Cocoapods builds
type CocoapodsModule struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// Fetcher always returns pod for CocoapodsModule
func (m CocoapodsModule) Fetcher() string {
	return "pod"
}

// Package returns the package spec for CocoapodsModule
func (m CocoapodsModule) Package() string {
	return m.Name
}

// Revision returns the version spec for CocoapodsModule
func (m CocoapodsModule) Revision() string {
	return m.Version
}

// PodFileLock models Podfile.lock yaml file
/*
	Note: we set Pods to `[]interface{}` because Golang doesn't support ADT's.
	The PODS section of Podfile.lock is actually [](map[string][]string | string)
*/
type PodFileLock struct {
	Pods            []interface{}                `yaml:"PODS,omitempty"`         // transitive deps + path
	Dependencies    []string                     `yaml:"DEPENDENCIES,omitempty"` // top level deps
	CheckoutOptions map[string]map[string]string `yaml:"CHECKOUT OPTIONS,omitempty"`
}

// Read and parse `podfile.lock`
func (p *PodFileLock) initLockFile(filePath string) error {
	bytes, err := ioutil.ReadFile(filePath)
	if err != nil {
		return fmt.Errorf("could not read Podfile.lock : %s", err.Error())
	}

	err = yaml.Unmarshal(bytes, &p)
	if err != nil {
		return fmt.Errorf("could not parse Podfile.lock : %s", err.Error())
	}

	return nil
}

func extractModule(fullDepStr string) CocoapodsModule {
	outputMatchRe := regexp.MustCompile(`(` + podNameRegex + `)\s+\(([\w/\.\-\\+=\s]+)\)`)
	match := outputMatchRe.FindStringSubmatch(fullDepStr)

	return CocoapodsModule{
		Name:    strings.Split(match[1], "/")[0],
		Version: match[2],
	}
}

// used to grab the pod name from the line
func extractPodName(fullDepStr string) string {
	outputMatchRe := regexp.MustCompile(podNameRegex)
	match := outputMatchRe.FindStringSubmatch(fullDepStr)

	return strings.Split(match[0], "/")[0]
}

// Analyze parses the `podfile.lock` YAML file and analyzes
func (builder *CocoapodsBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	log.Logger.Debugf("Running Cocoapods analysis: %#v %#v", m, allowUnresolved)
	var podLockfile PodFileLock

	currentLockfile := filepath.Join(m.Dir, "Podfile.lock")
	err := podLockfile.initLockFile(currentLockfile)
	if err != nil {
		return nil, fmt.Errorf("could not read and initialize Podfile.lock at %s: %s", currentLockfile, err.Error())
	}

	topLevelDeps := make(map[string]bool)         // This is a "Set"
	gitDepMap := make(map[string]module.Locator)  // This is for git deps included in podfile
	allDepsMap := make(map[string]module.Locator) // This is the final map of pod Name to Locator

	// We get all top level deps for accurate Path data (PODS lists a flat dep list)
	for _, dep := range podLockfile.Dependencies {
		depName := extractPodName(dep)
		topLevelDeps[depName] = true
	}

	// We check if any of the deps included are actually git dependencies
	for depName, checkoutOption := range podLockfile.CheckoutOptions {
		if checkoutOption[":git"] != "" && checkoutOption[":commit"] != "" {
			depName = strings.Split(depName, "/")[0]
			gitDepMap[depName] = module.Locator{
				Fetcher:  "git",
				Project:  checkoutOption[":git"],
				Revision: checkoutOption[":commit"],
			}
		}
	}

	importMap := make(map[string][]string) // maps parent deps to all transitive deps
	var imports []builderutil.Imported
	root := module.Locator{
		Fetcher:  "root",
		Project:  "root",
		Revision: "",
	}

	// Pods in the yaml file can be either a string or a {string: []string}
	// It contains the Path and version data needed
	for _, directDep := range podLockfile.Pods {
		var currentLocator module.Locator
		var currentTransitiveDeps []interface{}
		var parentDep CocoapodsModule

		// here we attempt to cast to `map[interface{}]interface{}` then `string`
		if mapDep, isMap := directDep.(map[interface{}]interface{}); isMap {
			for dep, uncastedTransitiveDeps := range mapDep {
				transitiveDeps, transOk := uncastedTransitiveDeps.([]interface{})
				depStr, depOk := dep.(string)
				if !depOk || !transOk {
					return nil, fmt.Errorf("malformed Podfile.lock file")
				}
				currentTransitiveDeps = transitiveDeps
				parentDep = extractModule(depStr)
			}
		} else if stringDep, isString := directDep.(string); isString {
			parentDep = extractModule(stringDep)
		} else {
			return nil, fmt.Errorf("malformed Podfile.lock file")
		}

		// substitute with git dep if brought in through github
		if val, ok := gitDepMap[parentDep.Name]; ok {
			currentLocator = val
		} else {
			currentLocator = module.Locator{
				Fetcher:  "pod",
				Project:  parentDep.Name,
				Revision: parentDep.Version,
			}
		}
		// We set the locator of the dep here (if not already set)
		if _, depSet := allDepsMap[parentDep.Name]; !depSet {
			allDepsMap[parentDep.Name] = currentLocator
		}

		// add root as direct parent to import path if a top level dep (deduping occurs later on)
		if _, ok := topLevelDeps[parentDep.Name]; ok {
			imports = append(imports, builderutil.Imported{
				Locator: currentLocator,
				From:    append(module.ImportPath{}, root),
			})
		}

		// group transitive deps by parents
		if currentTransitiveDeps != nil {
			for _, dep := range currentTransitiveDeps {
				depStr, ok := dep.(string)
				if !ok {
					continue
				}
				currentTransitiveDepName := extractPodName(depStr)
				if parentDep.Name != currentTransitiveDepName { // Adjust may have Adjust/Core listed as a dep for example, so we ignore that
					importMap[parentDep.Name] = append(importMap[parentDep.Name], currentTransitiveDepName)
				}
			}
		}
	}

	// Now that we have the locator for all deps, lets add the correct imports
	for parentDep, transitiveDeps := range importMap {
		duplicateDepMap := make(map[string]bool)
		for _, dep := range transitiveDeps {
			if duplicateDepMap[dep] != true {
				duplicateDepMap[dep] = true
				imports = append(imports, builderutil.Imported{
					Locator: allDepsMap[dep],
					From:    module.ImportPath{root, allDepsMap[parentDep]},
				})
			}
		}
	}

	deps := builderutil.ComputeImportPaths(imports)

	log.Logger.Debugf("Done running Pod analysis: %#v", deps)
	return deps, nil
}

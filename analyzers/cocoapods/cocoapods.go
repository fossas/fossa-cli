// Package cocoapods implements Cocoapods analysis.
//
// A `BuildTarget` for Cocoapods is the path to the Podfile.
package cocoapods

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	PodCmd     string
	PodVersion string

	Options Options
}

type Options struct{}

func New(opts map[string]interface{}) (*Analyzer, error) {
	// Set Cocoapods context variables
	podCmd, podVersion, err := exec.Which("--version", os.Getenv("COCOAPODS_BINARY"), "pod")
	if err != nil {
		return nil, fmt.Errorf("could not find Cocoapods binary (try setting $COCOAPODS_BINARY): %s", err.Error())
	}

	// Parse and validate options.
	var options Options
	err = mapstructure.Decode(opts, &options)
	if err != nil {
		return nil, err
	}
	log.Logger.Debug("Decoded options: %#v", options)

	analyzer := Analyzer{
		podCmd:     podCmd,
		podVersion: podVersion,

		Options: options,
	}

	log.Logger.Debugf("%#v", analyzer)
	return &analyzer, nil
}

// Discover constructs modules in all directories with a `Podfile`.
func (a *Analyzer) Discover(dir string) ([]module.Module, error) {
	var modules []module.Module
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Logger.Debugf("Failed to access path %s: %s\n", path, err.Error())
		}

		if !info.IsDir() && info.Name() == "Podfile" {
			moduleName := filepath.Base(path)

			log.Logger.Debugf("Found Cocoapods package: %s (%s)", path, moduleName)
			relPath, _ := filepath.Rel(dir, path)
			modules = append(modules, module.Module{
				Name:        moduleName,
				Type:        pkg.Cocoapods,
				BuildTarget: filepath.Dir(relPath),
				Dir:         filepath.Dir(relPath),
			})
		}

		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("Could not find Ruby package manifests: %s", err.Error())
	}
	return modules, nil
}

// IsBuilt checks whether `Podfile.lock` exists
func (a *Analyzer) IsBuilt(m module.Module) (bool, error) {
	log.Logger.Debugf("Checking Cocoapods build: %#v", m)

	isBuilt, err := files.Exists(m.Dir, "Podfile.lock")
	if err != nil {
		return false, err
	}

	log.Logger.Debugf("Done checking Cocoapods build: %#v", isBuilt)
	return isBuilt, nil
}

func (a *Analyzer) Clean(m module.Module) (bool, error) {
	log.Logger.Debugf("Checking Cocoapods build: %#v", m)

	isBuilt, err := files.Exists(m.Dir, "Podfile.lock")
	if err != nil {
		return false, err
	}

	log.Logger.Debugf("Done checking Cocoapods build: %#v", isBuilt)
	return isBuilt, nil
}

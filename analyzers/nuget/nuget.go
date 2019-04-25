// Package nuget implements NuGet analysis.
//
// A `BuildTarget` for NuGet is the path to the project file (e.g. the *.csproj
// file).
package nuget

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/apex/log"
	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/buildtools/dotnet"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	dotNET  dotnet.DotNET
	Module  module.Module
	Options Options
}

type Options struct {
	Strategy string `mapstructure:"strategy"`
}

func New(m module.Module) (*Analyzer, error) {
	log.WithField("options", m.Options).Debug("constructing analyzer")
	// Set Bower context variables
	dotnetCmd, dotnetVersion, err := exec.Which("--version", os.Getenv("DOTNET_BINARY"), "dotnet")
	if err != nil {
		log.Warn("Cannot find .NET binary")
	}

	// Decode options
	var options Options
	err = mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}

	analyzer := Analyzer{
		dotNET: dotnet.DotNET{
			Version: dotnetVersion,
			Cmd:     dotnetCmd,
		},
		Module:  m,
		Options: options,
	}

	log.Debugf("analyzer: %#v", analyzer)
	return &analyzer, nil
}

var xmlProj = regexp.MustCompile(`.*\.(cs|x|vb|db|fs)proj$`)

func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	moduleMap := make(map[string]module.Module)
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.WithError(err).WithField("path", path).Debug("error while walking for discovery")
			return err
		}

		if !info.IsDir() {
			name := info.Name()
			dir := filepath.Dir(path)
			moduleName := dir
			target := dir
			currentModule, dirCovered := moduleMap[dir]

			// Module preference
			// 1. Package Reference
			// 2. Nuspec
			// 3. packages.config, project.json, paket.lock
			if xmlProj.MatchString(name) {
				// For *.{cs,x,vb,db,fs}proj files, use the first <RootNamespace> seen.
				var manifest dotnet.Manifest
				err := files.ReadXML(&manifest, path)
				if err != nil {
					return err
				}
				if n := manifest.Name(); n != "" {
					moduleName = n
				}
				target = path
			} else if strings.HasSuffix(name, ".nuspec") && (!dirCovered || !xmlProj.MatchString(currentModule.BuildTarget)) {
				// For *.nuspec files, use the <id>.
				var nuspec dotnet.NuSpec
				err := files.ReadXML(&nuspec, path)
				if err != nil {
					return err
				}
				if id := nuspec.Metadata.ID; id != "" {
					moduleName = id
				}
				target = path
			} else if (name == "packages.config" || name == "project.json" || name == "paket.lock") && !dirCovered {
				// For other module indicators, use the directory name.
				target = dir
			} else {
				return nil
			}

			moduleMap[dir] = module.Module{
				Name:        moduleName,
				Type:        pkg.NuGet,
				BuildTarget: target,
				Dir:         dir,
			}
			return nil
		}

		return nil
	})
	if err != nil {
		return nil, err
	}

	var modules []module.Module
	for _, module := range moduleMap {
		modules = append(modules, module)
	}
	return modules, nil
}

func (a *Analyzer) Clean() error {
	log.Warn("Clean is not implemented for NuGet")
	return nil
}

func (a *Analyzer) Build() error {
	return a.dotNET.Build(Dir(a.Module))
}

func (a *Analyzer) IsBuilt() (bool, error) {
	return files.Exists(Dir(a.Module), "obj", "project.assets.json")
}

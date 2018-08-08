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

	"github.com/fossas/fossa-cli/buildtools/dotnet"
	"github.com/fossas/fossa-cli/files"

	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/exec"
	"github.com/apex/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	Cmd     string
	Version string

	dotNET  dotnet.DotNET
	Module  module.Module
	Options Options
}

type Options struct {
	Target string
}

func New(m module.Module) (*Analyzer, error) {
	log.Logger.Debug("%#v", m.Options)
	// Set Bower context variables
	dotnetCmd, dotnetVersion, err := exec.Which("--version", os.Getenv("DOTNET_BINARY"), "dotnet")
	if err != nil {
		return nil, errors.Wrap(err, "could not find dotnet binary (try setting $DOTNET_BINARY)")
	}

	// Decode options
	var options Options
	err = mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}

	analyzer := Analyzer{
		Cmd:     dotnetCmd,
		Version: dotnetVersion,

		dotNET: dotnet.DotNET{
			Cmd: dotnetCmd,
		},
		Module:  m,
		Options: options,
	}

	log.Logger.Debugf("analyzer: %#v", analyzer)
	return &analyzer, nil
}

func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	var modules []module.Module
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Logger.Debugf("Failed to access path %s: %s\n", path, err.Error())
			return err
		}

		if !info.IsDir() {
			name := info.Name()
			dir := filepath.Dir(path)
			moduleName := name

			// TODO(#172): this will use the lexicographically first indicator in the
			// directory, but we actually want the _best_ indicator (i.e. we should
			// prefer a *.csproj over a *.nuspec when both are available).
			xmlProj := regexp.MustCompile(".*\\.(cs|x|vb|db|fs)proj")
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
			} else if strings.HasSuffix(name, ".nuspec") {
				// For *.nuspec files, use the <id>.
				var nuspec dotnet.NuSpec
				err := files.ReadXML(&nuspec, path)
				if err != nil {
					return err
				}
				if id := nuspec.Metadata.ID; id != "" {
					moduleName = id
				}
			} else if name == "packages.config" || name == "project.json" {
				// For other module indicators, use the directory name.
				moduleName = filepath.Base(dir)
			} else {
				// TODO: get modules from `sln` files via `dotnet sln list`?
				return nil
			}

			modules = append(modules, module.Module{
				Name:        moduleName,
				Type:        pkg.NuGet,
				BuildTarget: path,
				Dir:         dir,
			})
			return filepath.SkipDir
		}

		return nil
	})
	if err != nil {
		return nil, err
	}

	return modules, nil
}

func (a *Analyzer) Clean() error {
	log.Logger.Warning("Clean is not implemented for NuGet")
	return nil
}

func (a *Analyzer) Build() error {
	return a.dotNET.Build(Dir(a.Module))
}

func (a *Analyzer) IsBuilt() (bool, error) {
	return files.Exists(Dir(a.Module), "obj", "project.assets.json")
}

package fossa

import (
	"encoding/json"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// SourceUnit is the basic module unit of the FOSSA API.
type SourceUnit struct {
	Name     string
	Type     string
	Manifest string
	Build    SourceUnitBuild
}

// A SourceUnitBuild contains the build information of a SourceUnit.
type SourceUnitBuild struct {
	Artifact string
	Context  interface{}

	Succeeded bool
	Error     error `json:",omitempty"`

	Imports      []string
	Dependencies []SourceUnitDependency
}

// A SourceUnitDependency contains the dependency information of a SourceUnit.
type SourceUnitDependency struct {
	// Location
	Locator string   `json:"locator"`
	Imports []string `json:"imports,omitempty"`

	// Metadata
	Data *json.RawMessage `json:"data,omitempty"`

	// Context
	Depth              int      `json:"depth,omitempty"`
	Parent             string   `json:"parent,omitempty"`
	UnresolvedLocators []string `json:"unresolved_locators,omitempty"`
}

// SourceUnitType normalizes pkg.Types into SourceUnit types.
func SourceUnitType(t pkg.Type) (string, error) {
	// TODO: handle more normalizations.
	switch t {
	case pkg.NodeJS:
		return "commonjspackage", nil
	default:
		return t.String(), nil
	}
}

// Normalize transforms module.Modules into SourceUnits.
func Normalize(modules []module.Module) ([]SourceUnit, error) {
	var normalized []SourceUnit
	for _, analyzed := range modules {
		var deps []SourceUnitDependency
		for _, dep := range analyzed.Deps {
			var imports []string
			for _, i := range dep.Imports {
				imports = append(imports, LocatorOf(i.Resolved).String())
			}

			deps = append(deps, SourceUnitDependency{
				Locator: LocatorOf(dep.ID).String(),
				Imports: imports,
			})
		}

		normalizedType, err := SourceUnitType(analyzed.Type)
		if err != nil {
			return nil, errors.Wrap(err, "could not normalize analyzed module type")
		}

		var imports []string
		for _, i := range analyzed.Imports {
			imports = append(imports, LocatorOf(i.Resolved).String())
		}
		normalized = append(normalized, SourceUnit{
			Name:     analyzed.Name,
			Type:     normalizedType,
			Manifest: analyzed.BuildTarget,
			Build: SourceUnitBuild{
				Artifact:     "default",
				Succeeded:    true,
				Dependencies: deps,
				Imports:      imports,
			},
		})
	}
	return normalized, nil
}

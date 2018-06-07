package fossa

import (
	"encoding/json"

	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type SourceUnit struct {
	Name     string
	Type     string
	Manifest string
	Build    Build
}

type Build struct {
	Artifact string
	Context  interface{}

	Succeeded bool
	Error     error `json:",omitempty"`

	Dependencies []Dependency
}

type Dependency struct {
	// Location
	Locator string `json:"locator"`

	// Metadata
	Data *json.RawMessage `json:"data,omitempty"`

	// Context
	Depth              int      `json:"depth,omitempty"`
	Parent             string   `json:"parent,omitempty"`
	UnresolvedLocators []string `json:"unresolved_locators,omitempty"`
}

func NormalizeType(t pkg.Type) (string, error) {
	// TODO: handle more normalizations
	switch t {
	case pkg.NodeJS:
		return "commonjspackage", nil
	default:
		return t.String(), nil
	}
	return "", errors.Errorf("unknown module type: %s", t.String())
}

func Normalize(modules []module.Module) ([]SourceUnit, error) {
	var normalized []SourceUnit
	for _, analyzed := range modules {
		var deps []Dependency
		for _, dep := range analyzed.Deps {
			data, err := json.Marshal(dep)
			if err != nil {
				return nil, errors.Wrap(err, "could not marshal analyzed dependency")
			}

			deps = append(deps, Dependency{
				Locator: LocatorOf(dep.ID).String(),
				Data:    (*json.RawMessage)(&data),
			})
		}

		normalizedType, err := NormalizeType(analyzed.Type)
		if err != nil {
			return nil, errors.Wrap(err, "could not normalize analyzed module type")
		}

		normalized = append(normalized, SourceUnit{
			Name:     analyzed.Name,
			Type:     normalizedType,
			Manifest: analyzed.BuildTarget,
			Build: Build{
				Artifact:     "default",
				Succeeded:    true,
				Dependencies: deps,
			},
		})
	}
	return normalized, nil
}

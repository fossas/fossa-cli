package api

import (
	"encoding/json"

	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/module"
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

func NormalizeType(t module.Type) (string, error) {
	// TODO: handle more normalizations
	switch t {
	case module.Nodejs:
		return "commonjspackage", nil
	}
	return "", errors.Errorf("unknown module type: %s", string(t))
}

func Normalize(analysis []module.Analyzed) ([]SourceUnit, error) {
	var normalized []SourceUnit
	for _, analyzed := range analysis {
		var deps []Dependency
		for _, dep := range analyzed.Dependencies {
			data, err := json.Marshal(dep)
			if err != nil {
				return nil, errors.Wrap(err, "could not marshal analyzed dependency")
			}

			deps = append(deps, Dependency{
				Locator: dep.Locator.String(),
				Data:    (*json.RawMessage)(&data),
			})
		}

		normalizedType, err := NormalizeType(analyzed.Module.Type)
		if err != nil {
			return nil, errors.Wrap(err, "could not normalize analyzed module type")
		}

		normalized = append(normalized, SourceUnit{
			Name:     analyzed.Module.Name,
			Type:     normalizedType,
			Manifest: analyzed.Module.Target,
			Build: Build{
				Artifact:     "default",
				Context:      analyzed.Builder,
				Succeeded:    true,
				Dependencies: deps,
			},
		})
	}
	return normalized, nil
}

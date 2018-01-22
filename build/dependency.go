package build

import "encoding/json"

// Dependency represents a code library brought in by running a Build
type Dependency interface {

	// locator fields
	Fetcher() string
	Package() string
	Revision() string

	// metadata api
	// isResolved() bool
	// doResolve() error
	// getMetadata() error
}

type NormalizedDependency struct {
	// Location
	Locator string `json:"locator"`

	// Metadata
	Data *json.RawMessage `json:",omitempty"`

	// Context
	Depth              int      `json:"depth,omitempty"`
	Parent             string   `json:"parent,omitempty"`
	UnresolvedLocators []string `json:"unresolved_locators,omitempty"`
}

// Locator transforms Dependencies into locator strings
func Locator(d Dependency) string {
	return d.Fetcher() + "+" + d.Package() + "$" + d.Revision()
}

// Normalize transforms a Dependency into a serializable struct
func Normalize(d Dependency) NormalizedDependency {
	data, _ := json.Marshal(d)
	return NormalizedDependency{
		Locator: Locator(d),
		Data:    (*json.RawMessage)(&data),
	}
}

// Dedupe removes duplicates from a []Dependency
func Dedupe(dependencies []Dependency) []Dependency {
	dedupedDependenciesMap := make(map[string]Dependency)
	for i := 0; i < len(dependencies); i++ {
		dedupedDependenciesMap[Locator(dependencies[i])] = dependencies[i]
	}

	dedupedDependencies := []Dependency{}
	for _, dep := range dedupedDependenciesMap {
		dedupedDependencies = append(dedupedDependencies, dep)
	}
	return dedupedDependencies
}

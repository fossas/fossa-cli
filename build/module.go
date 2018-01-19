package build

import (
	"encoding/json"
	"errors"
	"strings"
)

// Module represents a unit of buildable code within a codebase
// Each Module must have a single entry point / manifest where a build can initiate
// Currently only single build context is supported per module
type Module struct {
	Name     string
	Type     string
	Manifest string           // Required, path to an entry point for the Module _relative_ to repo root. Used for origin paths & ID.
	Dir      string           `json:",omitempty"`
	Data     *json.RawMessage `json:",omitempty"`

	// TODO: add support for multiple builds via []Build
	Build Build `json:",omitempty"`
}

// Analyze runs all builds that have not yet succeeded
func (m *Module) Analyze(opts map[string]interface{}) error {
	if m.Build.Context == nil {
		build := m.NewBuild("default")
		if build.Context == nil {
			return errors.New("Failed to assign build context")
		}
		m.Build = build
	}
	if m.Build.Succeeded {
		return nil
	}
	return m.Build.Run(m, opts)
}

// NewBuild creates a Build to a module given the build type
func (m *Module) NewBuild(name string) Build {
	build := Build{
		Artifact: name,
	}

	var ctx BuildContext

	switch strings.ToLower(m.Type) {
	case "commonjspackage":
		jsCtx := CommonJSContext{}
		ctx = &jsCtx
	case "bowerpackage":
		bowerCtx := BowerContext{}
		ctx = &bowerCtx
	case "rubygem":
		gemCtx := GemContext{}
		ctx = BuildContext(&gemCtx)
	case "mavenartifact":
		mvnCtx := MavenContext{}
		ctx = BuildContext(&mvnCtx)
		ctx = &gemCtx
	case "gopackage":
		goCtx := GolangContext{}
		ctx = &goCtx
	}

	build.Context = &ctx
	return build
}

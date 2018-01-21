package build

import (
	"encoding/json"
	"errors"

	. "github.com/fossas/fossa-cli/log"
)

// Build represents a task that builds a single artifact and generates dependency lists
type Build struct {
	Artifact string        // i.e. dev_server
	Context  *BuildContext `json:",omitempty"` // build context

	Succeeded    bool
	Error        error `json:",omitempty"`
	Dependencies []Dependency
}

// BuildContext describes instances that contain metadata and logic to run a build
type BuildContext interface {
	Initialize(m *Module, opts map[string]interface{})
	Verify(m *Module, opts map[string]interface{}) bool
	Build(m *Module, opts map[string]interface{}) error
}

// Run initializes and executes the build context
func (b *Build) Run(m *Module, opts map[string]interface{}) error {
	ctx := *b.Context
	if ctx == nil {
		return errors.New("build context has not been assigned yet; this usually means a build was not properly initiated")
	}

	ctx.Initialize(m, opts)

	if ctx.Verify(m, opts) == false && opts["install"].(bool) == false {
		return errors.New("build required; refusing to run unless --install flag is explicitly specified")
	}

	dat, _ := json.Marshal(*b.Context)
	Log.Debugf("running analysis with build context:\n%v", string(dat))
	if err := ctx.Build(m, opts); err != nil {
		b.Error = err
		b.Succeeded = false
		return err
	}

	// transform dependencies into locators
	for _, dep := range b.Dependencies {
		dep.Locator := Locator(dep)
	}

	b.Succeeded = true
	b.Error = nil
	return nil
}

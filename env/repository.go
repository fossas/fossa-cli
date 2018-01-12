package env

import (
	. "github.com/fossas/fossa-cli/module"
)

// Repository represents metadata about the VCS state of the CWD that fossa-cli is running in
// Repository objects encapsulate a set of modules that were discovered in them
type Repository struct {
	Name     string
	Type     string // VCS type (git, mecurial, etc...)
	URL      string
	Revision string
	Modules  []Module
}

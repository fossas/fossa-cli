package graph

import (
	"github.com/fossas/fossa-cli/pkg"
)

type Deps struct {
	Direct     []pkg.Import
	Transitive map[pkg.ID]pkg.Package
}

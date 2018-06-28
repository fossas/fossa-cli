package graph

import (
	"github.com/fossas/fossa-cli/pkg"
)

type Graph map[interface{}][]interface{}

type Deps map[pkg.ID]pkg.Package

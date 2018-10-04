package pipenv

import (
	"strings"

	"github.com/fossas/fossa-cli/pkg"

	"github.com/fossas/fossa-cli/files"
)

// DefaultPackages is a struct used to unmarshall the default production dependencies
// for a pipenv project from JSON. This does not include development dependencies.
type DefaultPackages struct {
	Default map[string]Requirement
}

// Requirement is a struct used to unmarshall Pipfile packages from JSON.
type Requirement struct {
	Name     string `json:"name"`
	Revision string `json:"version"`
	Operator string
}

func (r Requirement) String() string {
	return r.Name + r.Operator + r.Revision
}

// FromFile reads from a Pipfile and returns an array of []pkg.Import.
func FromFile(filename string) ([]pkg.Import, error) {
	var defaultPackages DefaultPackages
	var imports []pkg.Import

	err := files.ReadJSON(&defaultPackages, filename)
	if err != nil {
		return imports, err
	}

	var reqs []Requirement
	operators := []string{"==", "<=", ">=", ">", "<", "!="}
	for a, b := range defaultPackages.Default {
		b.Name = a
		for _, op := range operators {
			sections := strings.Split(b.Revision, op)
			if len(sections) == 2 {
				b.Revision = sections[1]
				b.Operator = op
				break
			}
		}
		reqs = append(reqs, b)
	}

	for _, req := range reqs {
		imports = append(imports, pkg.Import{
			Target: req.String(),
			Resolved: pkg.ID{
				Type:     pkg.Python,
				Name:     req.Name,
				Revision: req.Revision,
			},
		})
	}

	return imports, nil
}

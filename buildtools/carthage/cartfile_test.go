package carthage_test

import (
	"encoding/json"
	"fmt"
	"path/filepath"
	"testing"

	"github.com/fossas/fossa-cli/buildtools/carthage"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/stretchr/testify/assert"
)

func TestCartfileParsing(t *testing.T) {
	carthagePackage, err := carthage.FromResolvedCartfile("TestProject", filepath.Join("testdata", "testproject"))
	assert.NoError(t, err)

	data, err := json.Marshal(carthagePackage)
	assert.NoError(t, err)
	fmt.Printf("Imports: %s", string(data))

	deps := make(map[pkg.ID]pkg.Package)
	carthage.RecurseDeps(deps, carthagePackage)

	assert.Len(t, deps, 5, "Deps length should be 5")

	for _, dep := range deps {
		if dep.ID.Name == "https://github.com/Quick/Nimble" && dep.ID.Revision == "v7.1.3" {
			assert.Len(t, dep.Imports, 2, "Should have 2 deps")
			assert.Contains(t, dep.Imports, pkg.Import{
				Target: "githubhttps://github.com/mattgallagher/CwlCatchExceptionb14c111e9b33cd142bd4bc75c482cfd5c3490923",
				Resolved: pkg.ID{
					Type:     pkg.Carthage,
					Name:     "https://github.com/mattgallagher/CwlCatchException",
					Revision: "b14c111e9b33cd142bd4bc75c482cfd5c3490923",
				},
			})
			assert.Contains(t, dep.Imports, pkg.Import{
				Target: "githubhttps://github.com/mattgallagher/CwlPreconditionTesting1e62a726d54c743f4585233f08fcaac7307319b5",
				Resolved: pkg.ID{
					Type:     pkg.Carthage,
					Name:     "https://github.com/mattgallagher/CwlPreconditionTesting",
					Revision: "1e62a726d54c743f4585233f08fcaac7307319b5",
				},
			})
		}
	}

	fmt.Printf("Deps: %#v", deps)

}

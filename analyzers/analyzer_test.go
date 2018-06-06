package analyzers_test

import (
	"testing"

	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/pkg"
)

func TestAllAnalyzersImplemented(t *testing.T) {
	for _, pkgType := range pkg.AllTypes {
		_, err := analyzers.New(pkgType, nil)
		if err == analyzers.ErrAnalyzerNotImplemented || err == analyzers.ErrUnknownPackageType {
			t.Errorf("missing analyzer for type: %v", pkgType.String())
		}
	}
}

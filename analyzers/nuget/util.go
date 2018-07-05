package nuget

import (
	"path/filepath"

	"github.com/fossas/fossa-cli/module"
)

func Dir(m module.Module) string {
	if m.Dir == "" {
		return filepath.Dir(m.BuildTarget)
	}
	return m.Dir
}

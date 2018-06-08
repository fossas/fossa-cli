package gdm

import (
	"github.com/fossas/fossa-cli/files"
)

func UsedIn(dirname string) (bool, error) {
	return files.Exists(dirname, "Godeps")
}

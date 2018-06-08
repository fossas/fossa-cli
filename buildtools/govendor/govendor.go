package govendor

import (
	"github.com/fossas/fossa-cli/files"
)

func UsedIn(dirname string) (bool, error) {
	return files.Exists(dirname, "vendor", "vendor.json")
}

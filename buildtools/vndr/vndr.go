package vndr

import (
	"github.com/fossas/fossa-cli/files"
)

func UsedIn(dirname string) (bool, error) {
	return files.Exists(dirname, "vendor.conf")
}

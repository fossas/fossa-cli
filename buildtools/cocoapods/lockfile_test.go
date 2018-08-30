package cocoapods_test

import (
	"encoding/json"
	"fmt"
	"path/filepath"
	"testing"

	"github.com/fossas/fossa-cli/buildtools/cocoapods"
	"github.com/stretchr/testify/assert"
)

func TestLockfileParsing(t *testing.T) {
	lockfile, err := cocoapods.FromLockfile(filepath.Join("testdata", "Podfile.lock"))
	assert.NoError(t, err)

	data, err := json.Marshal(lockfile)
	fmt.Printf("%s", string(data))
}

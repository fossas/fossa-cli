package golang_test

import (
	"testing"

	"github.com/fossas/fossa-cli/analyzers/golang"
)

func TestUnvendorPathThatIsNotVendored(t *testing.T) {
	if "foobar" != golang.Unvendor("foobar") {
		t.Fail()
	}
}

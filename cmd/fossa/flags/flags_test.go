package flags_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/cmd/fossa/flags"
)

func TestCombine(t *testing.T) {
	fooFlag := cli.BoolFlag{Name: "foo", Usage: "bar"}
	helloFlag := cli.BoolFlag{Name: "hello", Usage: "world"}

	combined := flags.Combine(
		[]cli.Flag{fooFlag},
		[]cli.Flag{fooFlag, helloFlag},
	)
	assert.Equal(t, combined, []cli.Flag{fooFlag, helloFlag})

	assert.Panics(t, func() {
		flags.Combine(
			[]cli.Flag{fooFlag},
			[]cli.Flag{cli.BoolFlag{Name: "foo", Usage: "baz"}},
		)
	})
}

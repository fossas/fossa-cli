package npm_test

import (
    "testing"

    "github.com/fossas/fossa-cli/buildtools/npm"
)

func TestNoProdDeps(t *testing.T) {
    dir := "buildtools/npm/testdata"
    n, err := npm.New()
    if err != nil {
        t.Error("npm.New():", err)
    }

    err = n.Install(dir)
    if err != nil {
        t.Error("npm.Install():", err)
    }

    out, err := n.List(dir)
    if err != nil {
        t.Error("npm.List():", err)
    }

    for name, _ := range out.Dependencies {
        if name == "chai" {
            t.Fail()
        }
    }
}

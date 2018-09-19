package npm_test

import (
    "testing"

    "github.com/fossas/fossa-cli/buildtools/npm"
)

func TestNoProdDeps(t *testing.T) {
    dir := "buildtools/npm/testdata"
    n, e := npm.New()
    if e != nil {
        t.Error("npm.New():", e)
    }

    eInstall := n.Install(dir)
    if eInstall != nil {
        t.Error("npm.Install():", eInstall)
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

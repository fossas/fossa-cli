package main

import (
	"fmt"

	"github.com/BurntSushi/toml"
	"github.com/KyleBanks/depth"
	"github.com/bmatcuk/doublestar"
	logging "github.com/op/go-logging"
	"github.com/urfave/cli"
	git "gopkg.in/src-d/go-git.v4"
	yaml "gopkg.in/yaml.v2"
)

func main() {
	// This is here for import tracing + `dep` complaining about unused imports.
	var key toml.Key
	var tree depth.Tree
	doublestar.Match("", "")
	var backend logging.Backend
	var app cli.App
	var wt git.Worktree
	var item yaml.MapItem
	fmt.Printf("", key, tree, backend, app, wt, item)
}

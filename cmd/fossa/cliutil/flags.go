package cliutil

import "github.com/urfave/cli"

var GlobalFlags = []cli.Flag{
	cli.BoolFlag{Name: "no-ansi", Usage: "do not use interactive mode (ANSI codes)"},
	cli.BoolFlag{Name: "debug", Usage: "print debug information to stderr"},
}

func WithGlobalFlags(flags []cli.Flag) []cli.Flag {
	return append(flags, GlobalFlags...)
}

var ProgramFlags = []cli.Flag{}

var APIFlags = []cli.Flag{}

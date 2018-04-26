package common

import "github.com/urfave/cli"

var GlobalFlags = []cli.Flag{
	cli.BoolFlag{Name: "debug", Usage: "print debug information to stderr"},
	cli.BoolFlag{Name: "no-ansi", Usage: "do not use interactive mode (ANSI codes)"},
}

func WithGlobalFlags(flags []cli.Flag) []cli.Flag {
	return append(flags, GlobalFlags...)
}

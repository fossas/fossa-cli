// Package cli encompasses domain types for the CLI app.
package cli

type VCSType int

const (
	_ VCSType = iota
	Subversion
	Git
	Mercurial
	Bazaar
)

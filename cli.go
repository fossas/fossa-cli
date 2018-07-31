// Package cli encompasses domain types for the CLI app.
package cli

// VCSType represents a type of version control system.
type VCSType int

const (
	_ VCSType = iota
	Subversion
	Git
	Mercurial
	Bazaar
)

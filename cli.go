// Package cli encompasses domain types for the CLI app.
package cli

// VCS represents a type of version control system.
type VCS int

const (
	_ VCS = iota
	Subversion
	Git
	Mercurial
	Bazaar
)

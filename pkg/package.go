// Package pkg defines a generic software package.
package pkg

import "strings"

// An ID uniquely identifies a package.
type ID struct {
	// Type, Name, and Revision describe _what_ package is being identified. The
	// combination of these three fields uniquely identifies a single package on
	// a semantic level.
	Type     Type
	Name     string
	Revision string

	// Location is _where_ the package is found. For example, an NPM package with
	// the same name and revision may be found in two different registries.
	Location string
}

func (id *ID) String() string {
	return "id:" + escapeIDComponent(id.Type.String()) + ":" + escapeIDComponent(id.Name) + ":" + escapeIDComponent(id.Revision)
}

func escapeIDComponent(s string) string {
	return strings.Replace(s, ":", "\\:", -1)
}

// An Import is a combination of a (potentially unresolved) dependency target
// and the exact resolved pkg.ID of the dependency.
type Import struct {
	Target   string
	Resolved ID
}

type Imports []Import

type Deps map[ID]Package

// A Package represents a single package at a semantic level. The precise
// definition of "package" will vary from language to language.
type Package struct {
	ID ID

	Usage map[string]bool

	Imports []Import
}

type Qualifier int

const (
	Production Qualifier = iota
	Development
	Test
)

// An Author is a package author.
type Author struct {
	Name  string
	Email string
}

// A LicenseExpr is a raw string containing a SPDX-like license expression.
//
// LicenseExprs are not exactly the same as SPDX expressions, because they may
// contain license identifiers that are not valid SPDX licenses.
type LicenseExpr string

// TODO: Implement LicenseExpr as a parsed expression tree?

// TODO: Implement file-level granularity?
type File struct {
	Path       string
	Qualifiers string // For example, Go build tags
}

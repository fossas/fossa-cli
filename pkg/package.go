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

// A Package represents a single package at a semantic level. The precise
// definition of "package" will vary from language to language.
type Package struct {
	ID ID

	Authors          []Author      `json:",omitempty"`
	Description      string        `json:",omitempty"`
	DeclaredLicenses []LicenseExpr `json:",omitempty"`
	ProjectURL       string        `json:",omitempty"` // URL of the project (often the "homepage" URL)
	SourceCodeURL    string        `json:",omitempty"` // URL at which source code can be obtained (often a git repository)
	RegistryURL      string        `json:",omitempty"` // URL of the package's registry (e.g. if using private registries)
	DownloadURL      string        `json:",omitempty"` // URL at which binaries or compiled bundles can be downloaded

	// These are specific to the package's build, and tell us metadata about _how_
	// this package was used.
	BuildTool      string // Example: `yarn`
	OriginManifest string // Example: `./package.json`
	Registry       string // Example: `npm:npmjs.org`

	Imports []Import
	// A package's resolution strategy is set by the analyzer which resolves it,
	// and the meaning will vary depending on the analyzer used.
	Strategy string
}

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

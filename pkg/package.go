// Package pkg defines a generic software package.
package pkg

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

// An Import is a combination of a (potentially unresolved) dependency target
// and the exact resolved pkg.ID of the dependency.
type Import struct {
	Target   string
	Resolved *ID
}

// A Package represents a single package at a semantic level. The precise
// definition of "package" will vary from language to language.
type Package struct {
	ID ID

	Authors          []Author
	Description      string
	DeclaredLicenses []LicenseExpr
	SourceURL        string
	RegistryURL      string
	DownloadURL      string

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

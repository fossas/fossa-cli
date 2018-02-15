package module

// Locator is a string specifying a particular dependency and revision
type Locator string

// Dependency represents a code library brought in by running a Build
type Dependency interface {
	// Locator fields
	Fetcher() string
	Package() string
	Revision() string
}

// MakeLocator transforms Dependencies into locator strings
func MakeLocator(d Dependency) Locator {
	return Locator(d.Fetcher() + "+" + d.Package() + "$" + d.Revision())
}

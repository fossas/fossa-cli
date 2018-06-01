package module

// Dependency represents a code library brought in by running a Build
type Dependency struct {
	Locator
	Hashes
	Via []ImportPath
}

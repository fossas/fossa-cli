package project

// GoOptions sets analyzer options for Go projects.
type GoOptions struct {
	BuildOS         string // The target build OS (for build tags).
	BuildArch       string // The target build architecture (for build tags).
	Strategy        string // See the Go analyzer documentation.
	LockfilePath    string // For non-standard lockfile locations.
	AllowUnresolved bool   // Allow unresolved revisions.
	// AllowUnresolvedPrefix string // If set, restricts unresolved revisions to only those that match the prefix.
	// SkipTrace             bool   // If true, skips dependency tracing.
}

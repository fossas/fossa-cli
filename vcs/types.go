package vcs

// VCS represents a type of version control system.
type VCS int

const (
	_ VCS = iota
	Subversion
	Git
	Mercurial
	Bazaar
)

var Types = [4]VCS{
	Subversion,
	Git,
	Mercurial,
	Bazaar,
}

func MetadataFolder(vcs VCS) string {
	switch vcs {
	case Subversion:
		return ".svn"
	case Git:
		return ".git"
	case Mercurial:
		return ".hg"
	case Bazaar:
		return ".bzr"
	default:
		return ""
	}
}

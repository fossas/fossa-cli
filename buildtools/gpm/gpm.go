package gpm

import (
	"path"
	"strings"

	"github.com/fossas/fossa-cli/errutil"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/pkg"
)

// Lockfile is a GPM-format lockfile, implementing resolver.Resolver.
type Lockfile map[string]pkg.Import

// Resolve looks up packages as if the lockfile contained repository roots.
func (l Lockfile) Resolve(importpath string) (pkg.Import, error) {
	for p := importpath; p != "." && p != "/"; p = path.Dir(p) {
		rev, err := l.ResolveStrict(p)
		if err == errutil.ErrNoRevisionForPackage {
			continue
		}
		if err != nil {
			return pkg.Import{}, err
		}
		return rev, nil
	}
	return pkg.Import{}, errutil.ErrNoRevisionForPackage
}

// ResolveStrict looks up packages as if the lockfile contained import paths.
func (l Lockfile) ResolveStrict(importpath string) (pkg.Import, error) {
	rev, ok := l[importpath]
	if !ok {
		return pkg.Import{}, errutil.ErrNoRevisionForPackage
	}
	rev.Resolved.Name = importpath
	return rev, nil
}

// New reads and parses a GPM-format lockfile.
func New(pathElems ...string) (Lockfile, error) {
	// Read lockfile.
	data, err := files.ReadFile(pathElems...)
	if err != nil {
		return nil, err
	}

	// Parse lockfile.
	lockfile := make(map[string]pkg.Import)
	s := string(data)
	lines := strings.Split(s, "\n")
	for _, line := range lines {
		// Remove comments.
		commentStart := strings.Index(line, "#")
		if commentStart != -1 {
			line = line[:commentStart]
		}

		// Remove whitespace.
		line = strings.TrimSpace(line)

		// Skip empty lines.
		if line == "" {
			continue
		}

		// Parse fields: <import path> <revision> [repository]
		fields := strings.Fields(line)
		repo := ""
		if len(fields) == 3 {
			repo = fields[2]
		}
		lockfile[fields[0]] = pkg.Import{
			Resolved: pkg.ID{
				Type:     pkg.Go,
				Name:     fields[0],
				Revision: fields[1],
				Location: repo,
			},
		}
	}

	return lockfile, nil
}

// UsedIn checks whether gpm is used conventionally within a project folder.
func UsedIn(dirname string) (bool, error) {
	return files.Exists(dirname, "Godeps")
}

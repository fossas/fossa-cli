package gpm

import (
	"path"
	"strings"

	"github.com/fossas/fossa-cli/buildtools"
	"github.com/fossas/fossa-cli/files"
	"github.com/apex/log"
	"github.com/fossas/fossa-cli/pkg"
)

// Lockfile is a GPM-format lockfile, implementing resolver.Resolver.
type Lockfile map[string]pkg.Import

// Resolve looks up packages as if the lockfile contained repository roots.
func (l Lockfile) Resolve(importpath string) (pkg.Import, error) {
	for p := importpath; p != "." && p != "/"; p = path.Dir(p) {
		rev, err := l.ResolveStrict(p)
		if err == buildtools.ErrNoRevisionForPackage {
			continue
		}
		if err != nil {
			return pkg.Import{}, err
		}
		return rev, nil
	}
	return pkg.Import{}, buildtools.ErrNoRevisionForPackage
}

// ResolveStrict looks up packages as if the lockfile contained import paths.
func (l Lockfile) ResolveStrict(importpath string) (pkg.Import, error) {
	rev, ok := l[importpath]
	if !ok {
		return pkg.Import{}, buildtools.ErrNoRevisionForPackage
	}
	rev.Resolved.Name = importpath
	return rev, nil
}

// FromFile reads and parses a GPM-format lockfile.
func FromFile(filename ...string) (Lockfile, error) {
	// Read lockfile.
	data, err := files.Read(filename...)
	if err != nil {
		return nil, err
	}

	// Parse lockfile.
	lockfile := make(map[string]pkg.Import)
	s := string(data)
	lines := strings.Split(s, "\n")
	log.Logger.Debugf("lines: %#v", lines)
	for _, line := range lines {
		log.Logger.Debugf("line: %#v", line)
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
		log.Logger.Debugf("fields: %#v", fields)
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

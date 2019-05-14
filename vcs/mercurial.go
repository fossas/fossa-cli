package vcs

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
)

// MercurialRepository implements the System interface.
type MercurialRepository struct {
	dir     string
	project string
	head    Revision
}

// NewMercurialRepository uses the Mercurial repository's metadata at dir to identify the codebase.
func NewMercurialRepository(dir string) (*MercurialRepository, error) {
	cmd, _, err := exec.Which("--version", os.Getenv("HG_BINARY"), "hg")
	if err != nil {
		return nil, errors.Wrap(err, "could not find Mercurial binary")
	}

	branchStdout, _, err := exec.Run(exec.Cmd{
		Name: cmd,
		Argv: []string{"branch"},
		Dir:  dir,
	})
	if err != nil {
		return nil, errors.Wrapf(err, "could not run `%s branch`", cmd)
	}
	branch := strings.TrimSpace(branchStdout)

	// hg log -l 1 -r . --template '{node}'
	revisionID, _, err := exec.Run(exec.Cmd{
		Name: cmd,
		Argv: []string{"log", "-l", "1", "-r", ".", "--template", "{node}"},
		Dir:  dir,
	})
	if err != nil {
		return nil, errors.Wrapf(err, "could not get latest revision ID")
	}

	projectURL, err := runHgPaths(cmd, dir, "default")
	if err != nil || projectURL == "" {
		// Check what the path names are.
		pathNames, _, err := exec.Run(exec.Cmd{
			Name: cmd,
			Argv: []string{"paths", "-q"},
			Dir:  dir,
		})
		if err != nil || strings.TrimSpace(pathNames) == "" {
			projectPath, err := filepath.Abs(dir)
			if err != nil {
				return nil, errors.Wrap(err, "could not resolve absolute path to project")
			}
			projectURL = filepath.Base(projectPath)
			log.Warnf("Could not identify the project URL or name of remote repository. Using %q", projectURL)
		} else {
			// There is a path name. Use the first one listed.
			projectURL, err = runHgPaths(cmd, dir, strings.Split(pathNames, "\n")[0])
			if err != nil {
				return nil, errors.Wrapf(err, "could not determine the project's repository URL")
			}
		}
	}

	return &MercurialRepository{
		dir:     dir,
		project: projectURL,
		head: Revision{
			Branch:     branch,
			RevisionID: revisionID,
		},
	}, nil
}

func (m MercurialRepository) Project() string { return m.project }

func (m MercurialRepository) Head() Revision { return m.head }

func runHgPaths(cmd, dir, pathName string) (string, error) {
	url, _, err := exec.Run(exec.Cmd{
		Name: cmd,
		Argv: []string{"paths", pathName},
		Dir:  dir,
	})
	return strings.TrimSpace(url), err
}

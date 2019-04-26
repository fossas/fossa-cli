package vcs

import (
	"encoding/xml"
	"os"
	"strings"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
)

// SubversionRepository implements the System interface.
type SubversionRepository struct {
	dir  string
	cmd  string
	info svnInfo
}

// NewSubversionRepository uses the Subversion repository's metadata at dir to identify the codebase.
func NewSubversionRepository(dir string) (*SubversionRepository, error) {
	cmd, _, err := exec.Which("--version", os.Getenv("SVN_BINARY"), "svn")
	if err != nil {
		return nil, errors.Wrap(err, "could not find svn binary")
	}
	stdout, _, err := exec.Run(exec.Cmd{
		Name: cmd,
		Argv: []string{"info", "--xml"},
		Dir:  dir,
	})
	if err != nil {
		return nil, errors.Wrapf(err, "could not run `%s info`", cmd)
	}

	repo := SubversionRepository{
		dir: dir,
		cmd: cmd,
	}

	if err = repo.info.unmarshalXML([]byte(stdout)); err != nil {
		return nil, err
	}
	return &repo, nil
}

func (s *SubversionRepository) Project() string { return s.info.Entry.URL }

func (s *SubversionRepository) Head() Revision {
	return Revision{
		Branch:     svnBranchFromInfo(&s.info),
		RevisionID: s.info.Entry.Revision,
	}
}

// svnBranchFromInfo extracts the name of the branch from the info provided.
func svnBranchFromInfo(info *svnInfo) string {
	relativeURL := strings.TrimPrefix(info.Entry.RelativeURL, "^")

	// trimmed has just what follows the path of the URL locating the project.
	trimmed := strings.TrimPrefix(
		strings.TrimPrefix(info.Entry.URL, info.Entry.Repository.Root),
		relativeURL,
	)

	// Branches are typically identified by being under this directory.
	branches := "/branches/"

	if strings.HasPrefix(trimmed, branches) {
		// This is an ordinary branch.
		trimmed = strings.TrimPrefix(trimmed, branches)
	} else {
		trimmed = strings.TrimPrefix(trimmed, "/")
	}

	if trimmed != "" {
		return trimmed
	}
	return "trunk"
}

// The svnInfo type represents the result of running `svn info --xml`.
type svnInfo struct {
	Entry infoEntry `xml:"entry"`
}

type infoEntry struct {
	Path string `xml:"path,attr"`

	// Revision is the latest revision ID, a numeric string.
	Revision string `xml:"revision,attr"`
	Kind     string `xml:"kind,attr"`

	// URL is the remote location from which the repo can be downloaded.
	URL         string `xml:"url"`
	RelativeURL string `xml:"relative-url"`
	Repository  struct {
		Root string `xml:"root"`
		Uuid string `xml:"uuid"`
	} `xml:"repository"`
	WcInfo struct {
		WcrootAbspath string `xml:"wcroot-abspath"`
		Schedule      string `xml:"schedule"`
		Depth         string `xml:"depth"`
	} `xml:"wc-info"`
}

func (s *svnInfo) unmarshalXML(data []byte) error {
	return xml.Unmarshal(data, s)
}

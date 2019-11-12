package bazel

import (
	"path/filepath"
	"strings"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

func Deps(file string, upload bool) (graph.Deps, error) {
	buildFile, err := files.Read(file)
	if err != nil {
		return graph.Deps{}, err
	}

	id := pkg.ID{}
	for _, line := range strings.Split(string(buildFile), "\n") {
		trimLine := strings.TrimSpace(line)
		if strings.HasPrefix(trimLine, "dbx_thirdparty_cc_library") {
			loc, err := fossa.UploadTarballDependency(filepath.Dir(file), upload, true)
			if err != nil {
				log.Debugf("error found trying to upload c++ dependency %s: %s", file, err.Error())
				break
			}

			id.Type = pkg.Raw
			id.Name = loc.Project
			id.Revision = loc.Revision
			break
		}

		if strings.HasPrefix(trimLine, "dbx_py_pypi_piplib") {
			id.Type = pkg.Python
		}

		if strings.HasPrefix(trimLine, "dbx_go_library") {
			id.Type = pkg.Go
		}

		if strings.HasPrefix(trimLine, "rust_library") {
			id.Type = pkg.Rust
		}

		if strings.HasPrefix(trimLine, "dbx_npm_library") {
			id.Type = pkg.NodeJS
		}

		if strings.HasPrefix(trimLine, "name") {
			id.Name = strings.TrimSuffix(strings.TrimPrefix(trimLine, `name = "`), `",`)
		}

		if strings.HasPrefix(trimLine, "pip_version") {
			id.Revision = strings.TrimSuffix(strings.TrimPrefix(trimLine, `pip_version = "`), `",`)
		}

		if strings.HasPrefix(trimLine, "version") {
			id.Revision = strings.TrimSuffix(strings.TrimPrefix(trimLine, `version = "`), `",`)
		}

		if strings.HasPrefix(trimLine, "npm_req") {
			npmReq := strings.TrimSuffix(strings.TrimPrefix(trimLine, `npm_req = "`), `",`)
			npmSpec := strings.Split(npmReq, "@")
			if len(npmSpec) > 0 {
				id.Name = npmSpec[0]
			}

			if len(npmSpec) > 1 {
				id.Revision = npmSpec[1]
			}
		}
	}

	if id.Type == 0 {
		log.Debugf("Unable to find dependency information for file %s", file)
		return graph.Deps{}, nil
	}

	return graph.Deps{
		Direct:     []pkg.Import{pkg.Import{Resolved: id}},
		Transitive: map[pkg.ID]pkg.Package{id: pkg.Package{ID: id}},
	}, nil
}

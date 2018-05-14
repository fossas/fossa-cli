package builders

import (
	"archive/tar"
	"compress/gzip"
	"encoding/json"
	"errors"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/bmatcuk/doublestar"

	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

// VendoredArchiveBuilder implements Builder for projects that have archive-formatted
// rawDependencies encapsulating another supported build system
type VendoredArchiveBuilder struct {
	RpmCmd     string
	RpmVersion string
}

// Initialize gathers environment context.
func (builder *VendoredArchiveBuilder) Initialize() error {
	// TODO: gather information about command names and versions.
	return nil
}

// returns locators
func findNodePkgs(path string) []string {
	return nil
}

// Build does nothing for VendoredArchives
func (builder *VendoredArchiveBuilder) Build(m module.Module, force bool) error {
	return errors.New("VendoredArchive packages are expected to be pre-built")
}

// Analyze scans for archives and inspects them
func (builder *VendoredArchiveBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	// create tmp dir
	// find all archives
	// for each archive, extract into tmp dir and run package searcher
	// glob for deps and return them as packages

	// extractedArchivePaths := []string{}
	var rawDependencies []nodeManifest

	log.Debugf("Looking for tar files...")
	tarFiles, err := doublestar.Glob(filepath.Join(m.Dir, "**", "*.{tar.gz,tgz}"))
	if err != nil {
		return nil, err
	}
	log.Debugf("Found %#v tar files.", len(tarFiles))

	for i := 0; i < len(tarFiles); i++ {
		// Open the tar archive for reading.
		f, err := os.Open(tarFiles[i])
		if err != nil {
			log.Warningf("Unable to open tarfile: %#v", tarFiles[i])
		}
		defer f.Close()

		gzf, err := gzip.NewReader(f)
		if err != nil {
			log.Warningf("Gzip error: %#v", err)
			break
		}

		tr := tar.NewReader(gzf)

		// Iterate through the files in the archive.
		for {
			hdr, err := tr.Next()
			if err == io.EOF {
				break
			}

			if err != nil {
				log.Warningf("Tar error: %#v", err)
				return nil, err
			}

			if strings.HasSuffix(hdr.Name, "package.json") {
				log.Debugf("Found node module: %#v", hdr.Name)
				nodeManifestBuffer := make([]byte, hdr.Size)

				_, err := io.ReadFull(tr, nodeManifestBuffer)
				if err != nil {
					log.Warningf("Error reading node module: %#v", hdr.Name)
					break
				}
				var nodeModule nodeManifest
				if err := json.Unmarshal(nodeManifestBuffer, &nodeModule); err != nil {
					log.Warningf("Error parsing node module: %#v", hdr.Name)
					break
				}
				log.Debugf("Found node module: %#v", nodeModule)
				rawDependencies = append(rawDependencies, nodeModule)
				break
			}
		}
	}

	var castedDependencies []module.Dependency
	for i, d := range rawDependencies {
		castedDependencies[i] = module.Dependency{
			Locator: module.Locator{
				Fetcher:  "npm",
				Project:  d.Name,
				Revision: d.Version,
			},
			Via: nil,
		}
	}
	return castedDependencies, nil
}

// IsBuilt always returns true assuming vendored deps are already included
func (builder *VendoredArchiveBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	return true, nil
}

// IsModule is not implemented
func (builder *VendoredArchiveBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for VendoredArchiveBuilder")
}

// DiscoverModules is not implemented for VendoredArchiveBuilder, instead it must be explicitly configured
func (builder *VendoredArchiveBuilder) DiscoverModules(dir string) ([]module.Config, error) {
	return []module.Config{}, nil
}

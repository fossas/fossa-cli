package npm

import "github.com/fossas/fossa-cli/files"

type PackageJSON struct {
	Name    string
	Version string

	Dependencies map[string]string
}

func ParseNodePackageManifest(filename string) (PackageJSON, error) {
	var manifest PackageJSON
	err := files.ReadJSON(&manifest, filename)
	if err != nil {
		return PackageJSON{}, err
	}
	return manifest, nil
}

func ParseNodeModulesFolder(dir string) {}

func ParseNodeModulesFromManifest(filename string) {}

type PackageLockJSON struct{}

func ParseNodePackageLockfile(filename string) {}

type NPMListJSON struct{}

func ParseNPMOutput(output string) {}

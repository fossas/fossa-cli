package ant

import (
	"archive/zip"
	"bufio"
	"encoding/xml"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/apex/log"
	"github.com/bmatcuk/doublestar"
	"github.com/gnewton/jargo"

	"github.com/fossas/fossa-cli/buildtools/maven"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

func Graph(dir string) (graph.Deps, *errors.Error) {
	jarFilePaths, err := doublestar.Glob(filepath.Join(dir, "*.jar"))
	if err != nil {
		return graph.Deps{}, errors.UnknownError(err, fmt.Sprintf("jar files could not be found in the directory `%s`", dir))
	}

	log.Debugf("Running Ant analysis: %#v", jarFilePaths)

	// traverse through libdir and and resolve jars
	var imports []pkg.Import
	depGraph := make(map[pkg.ID]pkg.Package)
	for _, jarFilePath := range jarFilePaths {
		locator, err := locatorFromJar(jarFilePath)
		if err == nil {
			imports = append(imports, pkg.Import{
				Resolved: locator,
			})
			depGraph[locator] = pkg.Package{
				ID: locator,
			}
		} else {
			log.Warnf("unable to resolve Jar: %s", jarFilePath)
		}
	}

	return graph.Deps{
		Direct:     imports,
		Transitive: depGraph,
	}, nil
}

// locatorFromJar resolves a locator from a .jar file by inspecting its contents.
func locatorFromJar(path string) (pkg.ID, *errors.Error) {
	log.Debugf("processing locator from Jar: %s", path)

	info, err := jargo.GetJarInfo(path)
	if err == nil {
		// first, attempt to resolve a pomfile from the META-INF directory
		var pomFilePath string
		for _, file := range info.Files {
			if strings.HasPrefix(file, "META-INF") && strings.HasSuffix(file, "pom.xml") && (pomFilePath == "" || len(pomFilePath) > len(file)) {
				pomFilePath = file
			}
		}

		pomFile, err := getPOMFromJar(path, pomFilePath)
		if err == nil {
			log.Debugf("resolving locator from pom: %s", pomFilePath)
			groupID := pomFile.GroupID
			if groupID == "" {
				groupID = pomFile.Parent.GroupID
			}
			return pkg.ID{
				Type:     pkg.Maven,
				Name:     groupID + ":" + pomFile.ArtifactID,
				Revision: pomFile.Version,
			}, nil
		} else {
			log.Debugf("%s", err)
		}

		// failed to decode pom file, fall back to META-INF
		manifest := *info.Manifest
		if manifest["Bundle-SymbolicName"] != "" && manifest["Implementation-Version"] != "" {
			log.Debugf("resolving locator from META-INF: %s", info.Manifest)
			return pkg.ID{
				Type:     pkg.Maven,
				Name:     manifest["Bundle-SymbolicName"], // TODO: identify GroupId
				Revision: manifest["Implementation-Version"],
			}, nil
		}
	}

	// fall back to parsing file name
	re := regexp.MustCompile("(-sources|-javadoc)?.jar$")
	nameParts := strings.Split(re.ReplaceAllString(filepath.Base(path), ""), "-")
	lenNameParts := len(nameParts)

	var parsedProjectName string
	var parsedRevisionName string

	if lenNameParts == 1 {
		parsedProjectName = nameParts[0]
	} else if lenNameParts > 1 {
		parsedProjectName = strings.Join(nameParts[0:lenNameParts-1], "-")
		parsedRevisionName = nameParts[lenNameParts-1]
	}

	if parsedProjectName == "" {
		return pkg.ID{}, &errors.Error{
			Type:            errors.Unknown,
			Troubleshooting: fmt.Sprintf("The following jar was unable to be parsed into a maven coordinates `%s`. Ensure that the jar either contains a pom file, a META-INF file, or the name itself can be used to construct the coordinates.", path),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/ant.md#analysis",
		}
	}

	return pkg.ID{
		Type:     pkg.Maven,
		Name:     parsedProjectName,
		Revision: parsedRevisionName,
	}, nil
}

func getPOMFromJar(path, pomPath string) (maven.Manifest, *errors.Error) {
	var pomFile maven.Manifest

	log.Debugf(path)
	if path == "" {
		return pomFile, errors.UnknownError(nil, "Empty path specified for jar.")
	}
	// https: //github.com/fossas/fossa-cli/blob/master/docs/integrations/ant.md#ant--ivy
	jarFile, err := os.Open(path)
	if err != nil {
		return pomFile, errors.UnknownError(err, fmt.Sprintf("The jar `%s` was unable to be opened. Try opening it yourself and ensuring that it can be read.", path))
	}

	defer jarFile.Close()

	zfi, err := jarFile.Stat()
	if err != nil {
		return pomFile, errors.UnknownError(err, fmt.Sprintf("Fileinfo for the jar `%s` was unable to be obtained. Try opening it yourself and ensuring that it can be read.", path))
	}

	zr, err := zip.NewReader(jarFile, zfi.Size())
	if err != nil {
		return pomFile, errors.UnknownError(err, fmt.Sprintf("The jar `%s` was unable to be obtained. Try opening it yourself and ensuring that it can be read.", path))
	}

	for _, f := range zr.File {
		// decode a single pom.xml directly from jar
		if f.Name == pomPath {
			rc, err := f.Open()
			if err != nil {
				return pomFile, errors.UnknownError(err, fmt.Sprintf("The jar `%s` was unable to be opened. Try opening it yourself and ensuring that it can be read.", path))
			}
			defer rc.Close()

			reader := bufio.NewReader(rc)
			decoder := xml.NewDecoder(reader)

			if err := decoder.Decode(&pomFile); err != nil {
				return pomFile, errors.UnknownError(err, fmt.Sprintf("An xml formatted pom file could not be obtained from the jar file `%s`. Try unpacking the jar and inspect it for a pom file.", path))
			}

			return pomFile, nil
		}
	}

	return pomFile, errors.UnknownError(nil, fmt.Sprintf("Unable to parse POM from jar `%s`.", path))
}

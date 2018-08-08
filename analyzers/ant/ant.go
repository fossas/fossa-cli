package ant

import (
	"archive/zip"
	"bufio"
	"encoding/xml"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/bmatcuk/doublestar"
	"github.com/gnewton/jargo"
	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/buildtools/maven"
	"github.com/fossas/fossa-cli/graph"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/apex/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// Analyzer implements build context for SBT builds
type Analyzer struct {
	AntCmd     string
	AntVersion string

	JavaCmd     string
	JavaVersion string

	Module  module.Module
	Options Options
}

type Options struct {
	LibDir string
}

// Initialize collects metadata on Java and SBT binaries
func New(m module.Module) (*Analyzer, error) {
	log.Logger.Debugf("Initializing Ant builder...")

	// Set Java context variables
	javaCmd, javaVersion, err := exec.Which("-version", os.Getenv("JAVA_BINARY"), "java")
	if err != nil {
		log.Logger.Warningf("Could not find Java binary (try setting $JAVA_BINARY): %s", err.Error())
	}

	// Set Ant context variables
	antCmd, antVersionOut, err := exec.Which("-version", os.Getenv("ANT_BINARY"), "ant")
	if err != nil {
		return nil, fmt.Errorf("could not find Ant binary (try setting $ANT_BINARY): %s", err.Error())
	}

	antVersionMatchRe := regexp.MustCompile(`version ([0-9]+\.[0-9]+.\w+)`)
	match := antVersionMatchRe.FindStringSubmatch(antVersionOut)

	antVersion := ""
	if len(match) == 2 {
		antVersion = match[1]
	}

	// Decode options.
	var options Options
	err = mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}

	return &Analyzer{
		AntCmd:     antCmd,
		AntVersion: antVersion,

		JavaCmd:     javaCmd,
		JavaVersion: javaVersion,

		Module:  m,
		Options: options,
	}, nil
}

// Clean is currently not implemented
func (a *Analyzer) Clean() error {
	return errors.New("Clean is not implemented for Ant")
}

// Build is currently not implemented
func (a *Analyzer) Build() error {
	return errors.New("Build is not implemented for Ant")
}

// Analyze resolves a lib directory and parses the jars inside
func (a *Analyzer) Analyze() (graph.Deps, error) {
	log.Logger.Debugf("Running Ant analysis: %#v in %s", a.Module, a.Module.Dir)

	libdir := "lib"
	if a.Options.LibDir != "" {
		libdir = a.Options.LibDir
	}

	log.Logger.Debugf("resolving ant libs in: %s", libdir)
	if ok, err := files.ExistsFolder(a.Module.Dir, libdir); !ok || err != nil {
		return graph.Deps{}, errors.New("unable to resolve library directory, try specifying it using the `modules.options.libdir` property in `fossa.yml`")
	}

	jarFilePaths, err := doublestar.Glob(filepath.Join(libdir, "*.jar"))
	if err != nil {
		return graph.Deps{}, err
	}

	log.Logger.Debugf("Running Ant analysis: %#v", jarFilePaths)

	// traverse through libdir and and resolve jars
	var imports []pkg.Import
	for _, jarFilePath := range jarFilePaths {
		locator, err := locatorFromJar(jarFilePath)
		if err == nil {
			// hashes, _ := GetHashes(jarFilePath)
			imports = append(imports, pkg.Import{
				Resolved: locator,
			})
		} else {
			log.Logger.Warningf("unable to resolve Jar: %s", jarFilePath)
		}
	}

	return graph.Deps{
		Direct: imports,
	}, nil
}

func getPOMFromJar(path string) (maven.Manifest, error) {
	var pomFile maven.Manifest

	log.Logger.Debugf(path)
	if path == "" {
		return pomFile, errors.New("invalid POM path specified")
	}

	jarFile, err := os.Open(path)
	if err != nil {
		return pomFile, err
	}

	defer jarFile.Close()

	zfi, err := jarFile.Stat()
	if err != nil {
		return pomFile, err
	}

	zr, err := zip.NewReader(jarFile, zfi.Size())
	if err != nil {
		return pomFile, err
	}

	for _, f := range zr.File {
		// decode a single pom.xml directly from jar
		if f.Name == path {
			rc, err := f.Open()
			if err != nil {
				return pomFile, err
			}
			defer rc.Close()

			reader := bufio.NewReader(rc)
			decoder := xml.NewDecoder(reader)

			if err := decoder.Decode(&pomFile); err != nil {
				return pomFile, err
			}

			return pomFile, nil
		}
	}

	return pomFile, errors.New("unable to parse POM from Jar")
}

// locatorFromJar resolves a locator from a .jar file by inspecting its contents
func locatorFromJar(path string) (pkg.ID, error) {
	log.Logger.Debugf("processing locator from Jar: %s", path)

	info, err := jargo.GetJarInfo(path)
	if err == nil {
		// first, attempt to resolve a pomfile from the META-INF directory
		var pomFilePath string
		for _, file := range info.Files {
			if strings.HasPrefix(file, "META-INF") && strings.HasSuffix(file, "pom.xml") && (pomFilePath == "" || len(pomFilePath) > len(file)) {
				pomFilePath = file
			}
		}

		pomFile, err := getPOMFromJar(pomFilePath)
		if err == nil {
			log.Logger.Debugf("resolving locator from pom: %s", pomFilePath)
			return pkg.ID{
				Type:     pkg.Maven,
				Name:     pomFile.GroupID + ":" + pomFile.ArtifactID,
				Revision: pomFile.Version,
			}, nil
		} else {
			log.Logger.Debugf("%s", err)
		}

		// failed to decode pom file, fall back to META-INF
		manifest := *info.Manifest
		if manifest["Bundle-SymbolicName"] != "" && manifest["Implementation-Version"] != "" {
			log.Logger.Debugf("resolving locator from META-INF: %s", info.Manifest)
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
		return pkg.ID{}, errors.New("unable to parse jar file")
	}

	return pkg.ID{
		Type:     pkg.Maven,
		Name:     parsedProjectName,
		Revision: parsedRevisionName,
	}, nil
}

// IsBuilt always returns true for Ant builds
func (a *Analyzer) IsBuilt() (bool, error) {
	return true, nil
}

// Discover returns a root build.xml if found, and build configs for all sub-projects otherwise
func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	_, err := os.Stat(filepath.Join(dir, "build.xml"))
	if err == nil {
		// find the root build, as it can invoke tasks sub-builds
		absDir, err := filepath.Abs(dir)
		if err != nil {
			absDir = dir
		}
		artifactName := filepath.Base(absDir)
		return []module.Module{
			{
				Name: artifactName,
				Dir:  "build.xml",
				Type: pkg.Ant,
			},
		}, nil
	}

	// no build.xml in root directory; find and parse all of them
	antFilePaths, err := doublestar.Glob(filepath.Join(dir, "**", "build.xml"))
	if err != nil {
		return nil, err
	}
	moduleConfigs := make([]module.Module, len(antFilePaths))
	for i, path := range antFilePaths {
		artifactName := filepath.Dir(path) // Use the dirname as it's impossible to reliably parse from build.xml
		moduleConfigs[i] = module.Module{
			Name: artifactName,
			Dir:  path,
			Type: pkg.Ant,
		}
	}
	return moduleConfigs, nil
}

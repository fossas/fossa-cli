package carthage

import (
	"fmt"
	"path/filepath"
	"strings"

	"github.com/apex/log"
	"github.com/rveen/ogdl"

	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/pkg"
)

type Package struct {
	Name         string // Name of project
	Version      string // Version of project
	Dependencies []Requirement
	Dir          string // directory of cartfile
}

type Requirement struct {
	Origin       string // "github" || "git" || "binary"
	Name         string
	Revision     string // tag/branch/commit
	CheckoutName string // if github repo, i.e. "Quick/Nimble", Name will be "Nimble"
}

// Attempt to construct a Package from a dep given the parent directory
func (r Requirement) Package(dir string) (Package, error) {
	log.Logger.Debugf("Attempting to build Cartfile with dep %#v", r.CheckoutName)
	var resolvedCartfile Package

	requirementDirectory := filepath.Join(dir, "Carthage/Checkouts", r.CheckoutName)

	log.Logger.Debugf("Checking for Cartfile.resolved at: %#v", requirementDirectory)
	hasResolvedCartfile, err := files.Exists(filepath.Join(requirementDirectory, "Cartfile.resolved"))

	if err != nil {
		log.Logger.Debugf("Error checking for resolved cartfile: %#v, %#v", requirementDirectory, err.Error())
		return resolvedCartfile, err
	}

	if hasResolvedCartfile == false {
		log.Logger.Debugf("Cartfile.resolved missing in: %#v, exiting.", requirementDirectory)
		return resolvedCartfile, fmt.Errorf("Cartfile.resolved missing in: %#v", requirementDirectory)
	}

	// get current Cartfile.resolved
	resolvedCartfile, cartfileErr := FromResolvedCartfile(r.CheckoutName, requirementDirectory)
	if cartfileErr != nil {
		log.Logger.Debugf("Error parsing Cartfile.resolved at %#v: %#v", requirementDirectory, err.Error())
		return resolvedCartfile, cartfileErr
	}

	return resolvedCartfile, nil
}

func (r Requirement) String() string {
	return r.Origin + r.Name + r.Revision
}

func fullName(origin string, url string) string {
	if origin == "github" {
		return "https://github.com/" + url
	} else {
		return url
	}
}

/*
From the docs: Cartfiles are a restricted subset of the Ordered Graph Data Language, and any standard OGDL tool should be able to parse them.
Because of this, we use an OGDL parsing library (rveen/ogdl), which parses a cartfile like so:
_
	github
		Quick/Nimble
			v7.1.3
	github
		facebook/ios-snapshot-test-case
			2.1.4
	github
		facebook/yoga
			1.9.0
	github
		jspahrsummers/xcconfigs
			0.9
*/
func FromResolvedCartfile(projectName string, dir string) (Package, error) {
	cartfilePath := filepath.Join(dir, "Cartfile.resolved")
	checkoutsDir := filepath.Join(dir, "Carthage/Checkouts")
	log.Logger.Debugf("Parsing Cartfile.resolved at %#v", cartfilePath)
	var cartfile Package
	cartfileGraph := ogdl.FromFile(cartfilePath)

	var allDependencies []Requirement

	// Parse the OGDL
	for _, originGraph := range cartfileGraph.Out {
		if originGraph.ThisType() != "string" {
			log.Logger.Warning("Malformed Origin in Cartfile")
		}

		origin := originGraph.ThisString()

		urlGraph := originGraph.Out[0] // only need to know about first elem
		name := fullName(origin, urlGraph.ThisString())

		var checkoutName string
		if origin == "github" {
			splitName := strings.Split(name, "/")
			checkoutName = splitName[len(splitName)-1] // ex. Quick/Nimble
		} else {
			checkoutName = name
		}

		depCheckoutsDir := filepath.Join(checkoutsDir, checkoutName)
		depCheckoutsDirExists, err := files.ExistsFolder(depCheckoutsDir)

		if err != nil {
			log.Logger.Warningf("Error checking for existence of dir: %#v", depCheckoutsDir)
			continue
		}

		if depCheckoutsDirExists == false {
			log.Logger.Debugf("Checkouts folder doesn't exist for %#v. Skipping dependency.", checkoutName)
			continue
		}

		revisionGraph := urlGraph.Out[0] // only need to know about first elem

		allDependencies = append(allDependencies, Requirement{
			Origin:       origin,
			Name:         name,
			CheckoutName: checkoutName,
			Revision:     revisionGraph.ThisString(),
		})
	}

	cartfile.Dependencies = allDependencies
	cartfile.Name = projectName
	cartfile.Dir = dir

	log.Logger.Debugf("Done parsing Cartfile.resolved")
	return cartfile, nil
}

func RecurseDeps(pkgMap map[pkg.ID]pkg.Package, p Package) {
	log.Logger.Debugf("Searching Carthage deps for project %#v", p.Name)
	for _, dep := range p.Dependencies {
		// Construct ID.
		id := pkg.ID{
			Type:     pkg.Carthage,
			Name:     dep.Name,
			Revision: dep.Revision,
		}
		// Don't process duplicates.
		_, ok := pkgMap[id]
		if ok {
			continue
		}

		// Get direct imports.
		var imports []pkg.Import

		// Get Transitive Dep Info
		newPackage, err := dep.Package(p.Dir)

		if err != nil {
			log.Logger.Debugf("Error parsing Cartfile.resolved at %#v: %#v. Continuing...", p.Dir, err.Error())
		} else {
			for _, i := range newPackage.Dependencies {
				imports = append(imports, pkg.Import{
					Target: i.String(),
					Resolved: pkg.ID{
						Type:     pkg.Carthage,
						Name:     i.Name,
						Revision: i.Revision,
					},
				})
			}
		}
		// Update map.
		pkgMap[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
		// Recurse in imports.
		RecurseDeps(pkgMap, newPackage)
	}
}

package sbt_test

import (
	"fmt"
	"io/ioutil"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/sbt"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/pkg"
)

/*
	├─┬ dep:one:1.0.0
	| └─┬ dep:three:3.0.0
  	|   └── dep:four:4.0.0
	└─┬ dep:two:2.0.0
	  ├─┬ dep:three:3.0.0
	  │ └── dep:four:4.0.0
	  └── dep:five:5.0.0
*/

func TestParseDependencyTree(t *testing.T) {
	var graph sbt.GraphML
	err := files.ReadXML(&graph, filepath.Join("testdata", "sbt_test_graph.xml"))
	assert.NoError(t, err)

	evicted, err := ioutil.ReadFile(filepath.Join("testdata", "sbt_test_evicted"))
	assert.NoError(t, err)

	imports, transitive, err := sbt.ParseDependencyGraph(graph.Graph, string(evicted))
	assert.NoError(t, err)

	for dep, imports := range transitive {
		fmt.Println(dep, imports)
	}

	assert.Equal(t, 2, len(imports))
	assertImport(t, imports, "dep:one", "1.0.0")
	assertImport(t, imports, "dep:two", "2.0.0")

	assert.Equal(t, 5, len(transitive))

	packageOne := findPackage(transitive, "dep:one", "1.0.0")
	assert.NotEmpty(t, packageOne)
	assert.Equal(t, 1, len(packageOne.Imports))
	assertImport(t, packageOne.Imports, "dep:three", "3.0.0")

	packageTwo := findPackage(transitive, "dep:two", "2.0.0")
	assert.NotEmpty(t, packageTwo)
	assert.Equal(t, 2, len(packageTwo.Imports))
	assertImport(t, packageTwo.Imports, "dep:three", "3.0.0")
	assertImport(t, packageTwo.Imports, "dep:five", "5.0.0")

	packageThree := findPackage(transitive, "dep:three", "3.0.0")
	assert.NotEmpty(t, packageThree)
	assert.Equal(t, 1, len(packageThree.Imports))
	assertImport(t, packageThree.Imports, "dep:four", "4.0.0")

	packageFour := findPackage(transitive, "dep:four", "4.0.0")
	assert.NotEmpty(t, packageFour)
	assert.Equal(t, 0, len(packageFour.Imports))

	packageFive := findPackage(transitive, "dep:five", "5.0.0")
	assert.NotEmpty(t, packageFive)
	assert.Equal(t, 0, len(packageFive.Imports))
}

func TestSanityCheckParseDependencyTree(t *testing.T) {
	var graph sbt.GraphML
	err := files.ReadXML(&graph, filepath.Join("testdata", "sbt_dependencygraphml-prisma.xml"))
	assert.NoError(t, err)

	evicted, err := ioutil.ReadFile(filepath.Join("testdata", "sbt_evicted_nocolor-prisma"))
	assert.NoError(t, err)

	_, pkgs, err := sbt.ParseDependencyGraph(graph.Graph, string(evicted))
	assert.NoError(t, err)

	for id, pkg := range pkgs {
		assert.Equal(t, id, pkg.ID)
	}
}

func TestFilterLines(t *testing.T) {
	// This is a sampling of example disallowed lines from `sbt_dependencytree-prisma-stdout_and_stderr`
	disallowed := []string{
		`Getting org.scala-sbt sbt 1.0.4  (this may take some time)...`,
		`downloading https://repo1.maven.org/maven2/org/scala-sbt/sbt/1.0.4/sbt-1.0.4.jar ...`,
		`	[SUCCESSFUL ] org.scala-sbt#sbt;1.0.4!sbt.jar (111ms)`,
		`:: retrieving :: org.scala-sbt#boot-app`,
		`	confs: [default]`,
		`	69 artifacts copied, 0 already retrieved (22031kB/75ms)`,
		`[info] Loading settings from plugins.sbt ...`,
		`[info] Loading global plugins from /home/fossa/.sbt/1.0/plugins`,
		`[info] Updating {file:/home/fossa/.sbt/1.0/plugins/}global-plugins...`,
		`[info] Done updating.`,
		`[info] Loading settings from plugins.sbt ...`,
		`[info] Loading project definition from /home/fossa/prisma/server/project`,
		`[info] Updating {file:/home/fossa/prisma/server/project/}server-build...`,
		`[info] downloading https://repo1.maven.org/maven2/commons-logging/commons-logging/1.2/commons-logging-1.2.jar ...`,
		`[info] 	[SUCCESSFUL ] commons-logging#commons-logging;1.2!commons-logging.jar (103ms)`,
		`[info] Done updating.`,
		`[warn] Found version conflict(s) in library dependencies; some are suspected to be binary incompatible:`,
		`[warn] Run 'evicted' to see detailed eviction warnings`,
		`[info] Compiling 2 Scala sources to /home/fossa/prisma/server/project/target/scala-2.12/sbt-1.0/classes ...`,
		`[info] Non-compiled module 'compiler-bridge_2.12' for Scala 2.12.4. Compiling...`,
		`[info]   Compilation completed in 9.09s.`,
		`[info] Done compiling.`,
		`[info] Loading settings from build.sbt ...`,
		`[info] Loading settings from build.sbt,version.sbt ...`,
		`[info] Resolving key references (27245 settings) ...`,
		`[info] Set current project to server (in build file:/home/fossa/prisma/server/)`,
		`[info] Updating gc-values`,
		`https://repo1.maven.org/maven2/org/scalatest/scalatest_2.12/3.0.4/scalatest_2.12-3.0.4.pom`,
		`	100.0% [##########] 5.2 KiB (29.7 KiB / s)`,
		`[info] Resolved gc-values dependencies`,
		`[info] Updating gc-values`,
		`[info] Resolved gc-values dependencies`,
		`[info] Updating authorg/maven2/com/pauldijou/jwt-core_2.12/0.14.1/jwt-core_2.12-0.14.1.pom`,
		`https://repo1.maven.org/maven2/com/pauldijou/jwt-core_2.12/0.14.1/jwt-core_2.12-0.14.1.pom`,
		`	100.0% [##########] 2.3 KiB (90.6 KiB / s)`,
		`[info] Fetched artifacts of gc-values`,
		`[info] Updating prisma-config`,
		`https://repo1.maven.org/maven2/org/yaml/snakeyaml/1.19/snakeyaml-1.19.pom`,
		`  100.0% [##########] 34.4 KiB (649.6 KiB / s)`,
		`[info] Resolved prisma-config dependencies`,
		`[info] Updating {file:/home/fossa/prisma/server/}shared-models...`,
		`                                             https://repo1.maven.org/maven2/com/rabbitmq/amqp-client/4.1.0/amqp-client-4.1.0.jar`,
		`[info] Fetching artifacts of api-connector-postgresql`,
	}

	for _, line := range disallowed {
		actual := sbt.FilterLine(line)
		assert.False(t, actual, line)
	}

	allowed := []string{
		"[info] 	* prisma",
	}

	for _, line := range allowed {
		actual := sbt.FilterLine(line)
		assert.True(t, actual, line)
	}
}
func findPackage(packages map[pkg.ID]pkg.Package, name, revision string) pkg.Package {
	for id := range packages {
		if id.Name == name && id.Revision == revision {
			return packages[id]
		}
	}
	return pkg.Package{}
}

func assertImport(t *testing.T, imports pkg.Imports, name, revision string) {
	for _, importedProj := range imports {
		if importedProj.Resolved.Name == name {
			if importedProj.Resolved.Revision == revision {
				return
			}
			assert.Fail(t, "found "+name+"@"+importedProj.Resolved.Revision+" instead of "+revision)
		}
	}
	assert.Fail(t, "missing "+name+"@"+revision)
}

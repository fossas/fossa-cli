package maven_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/maven"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

func TestMavenPOMFile(t *testing.T) {
	buildTarget := ""
	m := module.Module{
		Name:        "test",
		Type:        pkg.Maven,
		BuildTarget: buildTarget,
		Dir:         "test-data",
	}

	mavenModule := m

	analyzer, err := maven.New(mavenModule)
	assert.NoError(t, err)

	analyzed, err := analyzer.Analyze()
	assert.NoError(t, err)

	directDeps := analyzed.Direct

	compileTimeDepsExist := false
	runtTimeDepsExist := false
	testDepsExist := false

	for _, dep := range directDeps {
		depName := dep.Resolved.Name
		if depName == "com.amazonaws:aws-lambda-java-core" {
			compileTimeDepsExist = true
		}
		if depName == "com.thoughtworks.xstream:xstream" {
			runtTimeDepsExist = true
		}
		if depName == "junit:junit" {
			testDepsExist = true
		}
	}

	assert.True(t, compileTimeDepsExist, "Compile time dependency found")
	assert.True(t, runtTimeDepsExist, "Run time dependency found")
	assert.False(t, testDepsExist, "Test dependency not found")
}

func TestMavenDependencyScope(t *testing.T) {
	buildTarget := ""

	m := module.Module{
		Name:        "test",
		Type:        pkg.Maven,
		BuildTarget: buildTarget,
		Dir:         "test-data",
	}

	// default scope
	mavenModuleWithDefaultScope := m

	analyzerWithDefaultScope, err := maven.New(mavenModuleWithDefaultScope)
	assert.NoError(t, err)

	analyzedDefaultScope, err := analyzerWithDefaultScope.Analyze()
	assert.NoError(t, err)

	// test scope
	mavenModuleWithTestScope := m

	mavenModuleWithTestScope.Options = map[string]interface{}{
		"Scope": "test",
	}

	analyzerWithTestScope, err := maven.New(mavenModuleWithTestScope)
	assert.NoError(t, err)

	analyzedTestScope, err := analyzerWithTestScope.Analyze()
	assert.NoError(t, err)

	// runtime scope
	mavenModuleWithRuntimeScope := m

	mavenModuleWithRuntimeScope.Options = map[string]interface{}{
		"Scope": "runtime",
	}

	analyzerWithRuntimeScope, err := maven.New(mavenModuleWithRuntimeScope)
	assert.NoError(t, err)

	analyzedRuntimeScope, err := analyzerWithRuntimeScope.Analyze()
	assert.NoError(t, err)

	// checking whether the "scope" is "runtime" by default
	assert.Equal(t, analyzedDefaultScope, analyzedRuntimeScope)

	assert.NotEqual(t, analyzedDefaultScope, analyzedTestScope)
}

func TestMavenSubmodules(t *testing.T) {
	buildTarget := ":app"
	m := module.Module{
		Name:        "test",
		Type:        pkg.Maven,
		BuildTarget: buildTarget,
		Dir:         "test-submodules",
	}

	mavenModule := m

	analyzer, err := maven.New(mavenModule)
	assert.NoError(t, err)

	analyzed, err := analyzer.Analyze()
	assert.NoError(t, err)

	directDeps := analyzed.Direct

	compileTimeDepsExist := false
	runtTimeDepsExist := false
	testDepsExist := false

	for _, dep := range directDeps {
		depName := dep.Resolved.Name
		if depName == "com.amazonaws:aws-lambda-java-core" {
			compileTimeDepsExist = true
		}
		if depName == "org.apache.httpcomponents:httpclient" {
			runtTimeDepsExist = true
		}
		if depName == "junit:junit" {
			testDepsExist = true
		}
	}

	assert.True(t, compileTimeDepsExist, "Compile time dependency found")
	assert.True(t, runtTimeDepsExist, "Run time dependency found")
	assert.False(t, testDepsExist, "Test dependency not found")
}

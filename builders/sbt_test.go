package builders_test

import (
	"testing"

	"github.com/fossas/fossa-cli/builders"
)

func assert(t *testing.T, property, actual, expected string) {
	if actual != expected {
		t.Errorf("%s was %#v instead of %#v", property, actual, expected)
	}
}

func TestInitialize(t *testing.T) {
	builder := builders.SBTBuilder{}
	err := builder.Initialize()
	if err != nil {
		t.Fatalf("Could not initialize: %#v", err)
	}
	if builder.JavaCmd != "java" {
		assert(t, "JavaCmd", builder.JavaCmd, "java")
	}
	if builder.JavaVersion == "" {
		assert(t, "JavaVersion", builder.JavaVersion, "anything")
	}
	if builder.SBTCmd != "sbt" {
		assert(t, "SBTCmd", builder.SBTCmd, "sbt")
	}
	if builder.SBTVersion == "" {
		assert(t, "SBTVersion", builder.SBTVersion, "anything")
	}
}

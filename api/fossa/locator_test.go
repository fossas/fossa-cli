package fossa_test

import (
	"testing"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/stretchr/testify/assert"
)

func TestLocatorFetcher(t *testing.T) {
	testcases := []struct {
		Type    pkg.Type
		Fetcher string
	}{
		{pkg.Ant, "mvn"},
		{pkg.Bower, "bower"},
		{pkg.Cocoapods, "pod"},
		{pkg.Composer, "comp"},
		{pkg.Go, "go"},
		{pkg.Git, "git"},
		{pkg.Gradle, "mvn"},
		{pkg.Maven, "mvn"},
		{pkg.NodeJS, "npm"},
		{pkg.NuGet, "nuget"},
		{pkg.Python, "pip"},
		{pkg.Ruby, "gem"},
		{pkg.Scala, "mvn"},
		{pkg.Raw, "archive"},
	}
	for _, tc := range testcases {
		id := pkg.ID{
			Type: tc.Type,
		}
		assert.Equal(t, tc.Fetcher, fossa.LocatorOf(id).Fetcher, tc.Fetcher)
	}
}

package bower_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/bower"
)

func TestConfigFileDefaultDirectory(t *testing.T) {
	config, err := bower.ReadConfig(".")
	assert.NoError(t, err)
	assert.Equal(t, config.Directory, "bower_components")

	config, err = bower.ReadConfig("testdata")
	assert.NoError(t, err)
	assert.Equal(t, config.Directory, "bower_components")
}

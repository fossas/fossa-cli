package config

import (
	"time"

	"github.com/fossas/fossa-cli/module"
)

// DefaultConfig specifies the config for the default command
type DefaultConfig struct {
	Build bool
}

// BuildConfig specifies the config for the build command
type BuildConfig struct {
	Force bool
}

// AnalyzeConfig specifies the config for the analyze command
type AnalyzeConfig struct {
	Output          bool
	AllowUnresolved bool
}

// TestConfig specifies the config for the test command
type TestConfig struct {
	Timeout time.Duration
}

// UploadConfig specifies the config for the upload command
type UploadConfig struct {
	UseLocators bool
	Data        string
}

// ReportConfig specifies the config for the report command
type ReportConfig struct {
	Type string // Either "dependencies" or "licenses"
	Format string // Either "text" or "json"
}

// CLIConfig specifies the config available to the cli
type CLIConfig struct {
	APIKey   string
	Fetcher  string
	Project  string
	Revision string
	Branch   string
	Endpoint string
	Modules  []module.Config

	Debug       bool
	Interactive bool

	DefaultCmd DefaultConfig
	AnalyzeCmd AnalyzeConfig
	BuildCmd   BuildConfig
	TestCmd    TestConfig
	UploadCmd  UploadConfig
	ReportCmd  ReportConfig

	ConfigFilePath string
	Version        string
}

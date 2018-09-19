package node

import (
	"os"
	"path/filepath"
	"sync"

	"github.com/fossas/fossa-cli/files"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/test/testtools"
)

type Project struct {
	name   string
	url    string
	commit string
	env    map[string]string
}

type NodeInitializer struct {
	projects []Project
}

func New() NodeInitializer {
	return NodeInitializer{
		projects: projects,
	}
}

func (n NodeInitializer) BuildAll() error {
	testDir := n.FixtureDirectory()
	testDirExists, err := files.ExistsFolder(testDir)
	if err != nil {
		return err
	}
	if testDirExists {
		println(testDir + "already exists, skipping initialization step")
		return nil
	}

	var waitGroup sync.WaitGroup
	waitGroup.Add(len(n.projects))

	for _, project := range n.projects {
		// Cloning an installing are slow, so run in parallel for each project
		go func(proj Project) {
			defer waitGroup.Done()

			projectDir := filepath.Join(testDir, proj.name)
			err := testtools.Clone(projectDir, proj.url, proj.commit)
			if err != nil && err.Error() != "repository already exists" {
				println("failed to clone " + proj.name)
				println(err.Error())
				panic(err)
			}

			_, _, err = exec.Run(exec.Cmd{
				Name:    "npm",
				Argv:    []string{"install", "--production"},
				Dir:     projectDir,
				WithEnv: proj.env,
				Command: "npm",
			})
			if err != nil {
				println("failed to run npm install on " + proj.name)
				println(err.Error())
			}

			err = testtools.FossaInit(projectDir)
			if err != nil {
				println("failed to run fossa init on " + proj.name)
				println(err.Error())
				panic(err)
			}
		}(project)
	}

	waitGroup.Wait()
	return nil
}

func (n NodeInitializer) RemoveAll() error {
	testDir := n.FixtureDirectory()
	return os.RemoveAll(testDir)
}

func (n NodeInitializer) FixtureDirectory() string {
	return filepath.Join(os.TempDir(), "nodejs")
}

var projects = []Project{
	Project{
		name:   "puppeteer",
		url:    "https://github.com/GoogleChrome/puppeteer",
		commit: "b97bddf8e5750d20c6ba82392eebe2a3fd2dd218",
		env: map[string]string{
			"PUPPETEER_SKIP_CHROMIUM_DOWNLOAD": "1",
		},
	},
	Project{
		name:   "fakerjs",
		url:    "https://github.com/Marak/faker.js",
		commit: "3a4bb358614c1e1f5d73f4df45c13a1a7aa013d7",
		env:    map[string]string{},
	},
	Project{
		name:   "fastify",
		url:    "https://github.com/fastify/fastify",
		commit: "1b16a4c5e381f9292d3ac2c327c3bda4bd277408",
		env:    map[string]string{},
	},
	Project{
		name:   "nest",
		url:    "https://github.com/nestjs/nest",
		commit: "ce498e86150f7de4a260f0c393d47ec4cc920ea1",
		env:    map[string]string{},
	},
	Project{
		name:   "ohm",
		url:    "https://github.com/harc/ohm",
		commit: "8202eff3723cfa26522134e7b003cf31ab5de445",
		env:    map[string]string{},
	},
	Project{
		name:   "express",
		url:    "https://github.com/expressjs/express",
		commit: "b4eb1f59d39d801d7365c86b04500f16faeb0b1c",
		env:    map[string]string{},
	},
	Project{
		name:   "standard",
		url:    "https://github.com/standard/standard",
		commit: "bc02256fa2c03632e657248483c55a752e63e724",
		env:    map[string]string{},
	},
	Project{
		name:   "sodium-encryption",
		url:    "https://github.com/mafintosh/sodium-encryption",
		commit: "42a7cba0f97718157e8c7a386ef94ba31e16837a",
		env:    map[string]string{},
	},
	Project{
		name:   "request",
		url:    "https://github.com/request/request",
		commit: "8162961dfdb73dc35a5a4bfeefb858c2ed2ccbb7",
		env:    map[string]string{},
	},
}

package gradle

type Gradle struct {
	Cmd string
}

func (g *Gradle) Dependencies() {}

func (g *Gradle) Run(project, task, configuration string) error {}

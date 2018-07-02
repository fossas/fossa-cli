package nuget

type Analyzer struct {
	Cmd     string
	Version string
}

type Options struct {
}

func New(opts map[string]interface{}) (*Analyzer, error) {
	log.Logger.Debug("%#v", opts)
	// Set Bower context variables
	nugetCmd, nugetVersion, err := exec.Which("-v", os.Getenv("NUGET_BINARY"), "dotnet", "nuget")
	if err != nil {
		return nil, errors.Wrap(err, "could not find NuGet binary (try setting $NUGET_BINARY)")
	}

	// Decode options
	var options Options
	err = mapstructure.Decode(opts, &options)
	if err != nil {
		return nil, err
	}

	analyzer := Analyzer{
		Cmd:     nugetCmd,
		Version: nugetVersion,

		Options: options,
	}

	log.Logger.Debugf("analyzer: %#v", analyzer)
	return &analyzer, nil
}

func (*a Analyzer) Discover(dir string) ([]module.Module, error) {}

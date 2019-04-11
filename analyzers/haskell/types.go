package haskell

type Options struct {
	Strategy Strategy `mapstructure:"strategy"`
}

type Strategy string
const (
	CabalInstall Strategy = "cabal-install"
	Stack                 = "stack"
)


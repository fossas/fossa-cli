
## Building

Use [ghcup](https://gitlab.haskell.org/haskell/ghcup) to install the `cabal` cli tool and the ghc version we're using:

```sh
$ ghcup install-cabal
$ ghcup install 8.6
```

In the base directory, run `cabal build`

## Running

```sh
$ cabal run hscli -- scan -d path/to/basedir/
```

This will place `analysis.json` at `path/to/basedir/analysis.json`

## Testing

Configure the project to enable tests (this will rebuild dependencies the first time):
```sh
cabal configure --enable-tests
```

Run the tests:
```
cabal test
```

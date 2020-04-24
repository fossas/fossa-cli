## Installation

### MacOS (Darwin) or Linux amd64:
```bash
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/spectrometer/master/install.sh | bash
```

### Windows with Powershell:
```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; iex  ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/fossas/spectrometer/master/install.ps1'))
```

## Building

Use [ghcup](https://gitlab.haskell.org/haskell/ghcup) to install the `cabal` cli tool and the ghc version we're using:

```sh
$ ghcup install-cabal
$ ghcup install 8.8
$ ghcup set 8.8
```

In the base directory, run `cabal build`

## Running

```sh
$ cabal run fossa -- analyze -d path/to/basedir/ -o
```

This will produce analysis results on stdout

## Testing

Configure the project to enable tests
```sh
cabal configure --enable-tests
```

Run the tests:
```
cabal test
```

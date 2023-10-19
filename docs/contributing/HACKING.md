# Hacking

[Back to development doc homepage](README.md)

## Prerequisites

* lzma (libarchive on Mac OS X)
* pkg-config

On Macs you need to have installed the developer tooling using `xcode-select --install` or equivalent.

## Quickstart

Use [ghcup][ghcup] to install the `cabal` cli tool and the ghc version we're using:

```sh
$ ghcup install ghc 9.4
<long running output, about 4 min on my machine>
$ ghcup set ghc 9.4
$ cabal update
$ cabal build
```

### Apple Silicon

In previous GHC versions (8.10), `llvm` was required

### Quickstart Explanation

Ok, the quickstart worked for you, but why, and how?

> `ghcup install ghc 9.4`

When you install `ghcup`, `ghc` and `cabal-install` are installed automatically as part of the initial installation (see [Tools](#Tools) for descriptions of `ghc` and `cabal-install`).
The `ghc` version that is automatically installed may not be the correct version we use (though it may work just fine).  So we install the correct version with `ghcup install ghc 9.4`.
Currently, the best place to check the correct version is our CI build files (try `.github/workflows/build.yml`).

> `ghcup set ghc 9.4`

`ghcup` works by setting symlinks to the "active" version of the tool you're using.  Here, we're telling `ghcup` to set GHC 9.4 as the active GHC version.
Now, when you run `ghc`, you'll be running GHC 9.4.

> `cabal update`

Cabal caches a local index of packages (and their metadata) to try to resolve dependencies for builds.
This cache often goes out of date since new package versions are released often.  `cabal update` will refresh this cache.

> `cabal build`

This builds the actual project, and is a perfect sanity check for checking that you have the correct tools installed.
You'll use this command a lot.

### Building

In the base directory, run `cabal build`.  if you want to build the full binary, run `./vendor_download.sh` first (requires a FOSSA-org `GITHUB_TOKEN` set in the environment).

#### Note for Non-FOSSA users

Currently, building the full-featured binary requires the use of external binaries fetched from closed-source repos in the FOSSA organization (`fossas`).
If you build without these binaries, a compiler warning will be emitted.  If ghc's `-Werror` option is enabled, this will fail the build, and is therefore not enabled by default.

There is no supported way for Non-FOSSA users to obtain these binaries, though we are working on a solution to allow this in the future.  As a result, **we cannot accept pull requests from any forked repository**, since those builds will fail in CI.  This means that outside contributions are possible, but a FOSSA employee needs to cherry pick the commits and create a new PR with them.

### Running tests

> You will need to run `git lfs pull` to retrieve testdata stored in git lfs. If you do not have git-lfs installed on your system,
> you can refer to: https://github.com/git-lfs/git-lfs#getting-started for git-lfs installation.

Run the unit tests by running `cabal test unit-tests` in the base directory.

Integration tests require you first build test data by running `make build-test-data`.  Then you can run them with `cabal test integration-tests`.  Note that integration tests can take quite a while to run and do not have progress output.

Both test suites will be run when you execute `cabal test`.

## Tools

| name                                   | description                                                                                                  |
| -------------------------------------- | ------------------------------------------------------------------------------------------------------------ |
| [ghcup][ghcup]                         | Used to manage installed versions of ghc and cabal-install                                                   |
| ghc                                    | The haskell compiler (installed via ghcup)                                                                   |
| cabal-install                          | The package manager we use (installed via ghcup). Accessed via `cabal` on most setups.                       |
| [haskell-language-server][hls] ("HLS") | LSP server for haskell projects                                                                              |
| [hlint][hlint]                         | A linting + hints tool for haskell code. It provides really useful suggestions.  `hlint` is bundled with HLS |
| [fourmolu][fourmolu]                   | A haskell source code formatter. `fourmolu` is bundled with HLS                                              |

### Installing haskell-language-server

In VSCode:

- Install the "Haskell Language Server" (`haskell.haskell`) plugin in VSCode.
- In the Haskell extension settings, under `Haskell: Formatting Provider`, choose `fourmolu`

If you installed HLS in the old, complicated way, you can safely remove it.  HLS now bundles all of its needed tools.

You should also set the `FOSSA_SKIP_EMBED_FILE_IN_HLS` environment variable for HLS. This prevents HLS from embedding binaries, which helps to avoid a giant memory footprint for HLS.

You can also tell the Fourmolu plugin to use an external config. This ensures that it picks up our `fourmolu.yaml` file.

In VSCode, this is done by adding this to your `settings.json`:

```json
    "haskell.serverEnvironment": {
        "FOSSA_SKIP_EMBED_FILE_IN_HLS": true,
    },
    "haskell.plugin.fourmolu.config.external": true,
```

## Linting

`hlint` is built into HLS, and is enabled by default. hlint suggestions appear as warnings in the editor.

You can also use `make lint` to run the linter.

`make lint` is run in CI, any errors will prevent merging.

## Formatting

Built into HLS, we use `fourmolu` for formatting source code files. In the VSCode Haskell extension settings, under `Haskell: Formatting Provider`, choose `fourmolu`.

In VSCode, the formatter can be activated with the standard format-file and format-region actions.

Make sure to run the formatter on any files you modify. Enabling `Editor: Format On Save` in VSCode can make satisfying this requirement easier.

You can run `make fmt` to run the formatter on the entire codebase, or `make check-fmt` to dry-run the formatter.

`make fmt` is run in CI, any formatting errors will prevent merging.

Our makefile also has the ability to run the formatter from the CI image locally against your project.  To do this
run `make fmt-ci`, which will mount your project into the container used for checking formatting in CI, and then
run `make fmt` within the container.  This allows you to run the formater locally without having to startup an editor
or having to install the formatter yourself.  This also makes sure you're using the same version as CI.

## Docs

| name               | description                                                                    |
| ------------------ | ------------------------------------------------------------------------------ |
| [hoogle][hoogle]   | Search for type signatures or symbols                                          |
| [hackage][hackage] | Package repository; can be used to browse individual package docs ("haddocks") |

If on macOS, [dash](https://kapeli.com/dash) is a great tool that allows for downloading searchable package haddocks.

On linux, you can use [zeal](https://zealdocs.org/).  (Currently there is an issue with building third-party docsets, if you discover a solution to get e.g.: `aeson` docs in `zeal`, please file an issue or submit a PR to fix these docs.)

## Cheatsheets

### Cabal cheatsheet

| command                              | description                                                                |
| ------------------------------------ | -------------------------------------------------------------------------- |
| `cabal repl`                         | opens the ghci repl on the project                                         |
| `cabal build`                        | build fossa-cli                                                            |
| `cabal test`                         | build + run tests                                                          |
| `cabal run binary-name -- arg1 arg2` | build + run an executable named `binary-name`, and with args `arg1` `arg2` |

### GHCI cheatsheet

Use `cabal repl` to open ghci.

| command                    | description                                                  |
| -------------------------- | ------------------------------------------------------------ |
| `:r`/`:reload`             | reload the project                                           |
| `:t`/`:type <symbol>`      | query the type of a symbol                                   |
| `:i`/`:info <symbol>`      | query info about a symbol -- docs, where it was defined, etc |
| `:l`/`:load <Module.Name>` | load a specific file into the repl                           |

## FAQ/Troubleshooting

### Cabal is complaining about dependencies, and I don't understand it

Yeah, haskell tools can be a little over-explainy and use too many technicalities. Try these steps (one at a time):

- Run `cabal update`.  This solves most problems with dependencies and should be a go-to for these issues.
- Check your GHC version.  Some dependencies are baked-in (sort of) to the compiler, so make sure you're using the correct version.
- Update `ghcup`, then re-check all of your haskell tools' versions.  `ghcup tui` is a great interface for this, but you can use normal commands.

### I tried using fourmolu/hlint, and it choked on some syntax that builds fine

We use a fair amount of GHC extensions, which can greatly change the syntax of a file.  When the extensions are listed at the top of a file
using `{#- LANGUAGE GADTs -#}`-style syntax, these tools can easily pick that up.  But some extensions, like `TypeApplications`, are so ubiquitous
that we define them everywhere using cabal's `default-extensions` feature.  If these also extensions modify syntax (like `TypeApplications` does), then
these tools can choke, and sometimes pretty poorly.

Using these tools through HLS should prevent these issues, since HLS checks for build-system-provided extensions.  The `makefile` is also set up to
include the correct extensions where necessary, so running `make lint` or `make fmt` can be easier than running `hlint` or `fourmolu` directly.

### GHC/hlint is telling me to add/remove a language extension.  Is that safe?

Yes.  Missing language extensions are usually compile-time errors, and will be caught in CI.  Unused extensions are caught by hlint, and can be safely removed.
If, for any reason, GHC tells you add an extension, and hlint tells you to remove the extension you just added, keep it there and ignore hlint.  You should also file
an issue in this repository for that scenario, since we may be able to fix that.

[fourmolu]: https://github.com/fourmolu/fourmolu
[ghcup]: https://www.haskell.org/ghcup
[hackage]: https://hackage.haskell.org/
[hlint]: https://github.com/ndmitchell/hlint
[hls]: https://github.com/haskell/haskell-language-server
[hoogle]: https://hoogle.haskell.org/

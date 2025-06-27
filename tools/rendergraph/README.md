# fossa-rendergraph

Utility to render the output of `fossa analyze -o` as a graph.

This program isn't _guaranteed_ to not be broken by new FOSSA CLI releases,
but we'll try to keep it from doing so especially since we try to keep output of FOSSA CLI stable.

# Installation from source

0. Install `rust`: https://www.rust-lang.org/tools/install
1. Clone this repo locally
2. Run `cargo install --path rendergraph` from the parent directory.
3. Use as `fossa analyze -o | rendergraph` (you can also `cat output.json | rendergraph`- any stdin works)

# What to expect

Direct deps are roots, and tree branches leading off of them are their transitive deps.

Cycles are denoted by the literal `cycle ->` followed by the name of the node that would have had a cycling edge.

For example, a cycle of `a -> b -> c -> a` reads as:
```
a
└─ b
   └─ c
      └─ cycle -> a
```

This tool outputs all dependencies found by FOSSA and doesn't currently account for FOSSA's "unused dependencies" graph pruning.

# Future work

A list of things I've thought of to add. Have an idea? Add it to this list via PR!

- [ ] Account for graph pruning (use source units to do this)
- [ ] Support graphviz output

# Example output

Note: This output was truncated for readability.

```
; fossa analyze \
  ~/projects/fossa-cli \
  --only-target gomod --only-target cabal \
  -o | rendergraph

[ INFO] Analyzing cabal project at /Users/kit/projects/fossa-cli/
[ INFO] Analyzing gomod project at /Users/kit/projects/fossa-cli/
[ INFO]
[ INFO] Scan Summary
[ INFO] ------------
[ INFO] fossa-cli version 3.2.9 (revision 2e79a284caee compiled with ghc-8.10)
[ INFO]
[ INFO] 2 projects scanned;  0 skipped,  2 succeeded,  0 failed,  0 analysis warnings
[ INFO]
[ INFO] * cabal project in "/Users/kit/projects/fossa-cli/": succeeded
[ INFO] * gomod project in "/Users/kit/projects/fossa-cli/": succeeded
[ INFO] ------------

----- Project: "/Users/kit/projects/fossa-cli/" (cabal) -----
      186 total deps, 67 direct

HUnit@1.6.2.0
└─ call-stack@0.4.0
aeson@1.5.6.0
├─ vector@0.12.3.1
│  └─ primitive@0.7.3.0
├─ uuid-types@1.0.5
│  ├─ random@1.2.1
│  │  └─ splitmix@0.1.0.4
│  └─ hashable@1.3.5.0
├─ unordered-containers@0.2.18.0
│  └─ hashable@1.3.5.0
├─ time-compat@1.9.6.1
│  ├─ hashable@1.3.5.0
│  └─ base-orphans@0.8.6
├─ these@1.1.1.1
│  ├─ hashable@1.3.5.0
│  └─ assoc@1.0.2
│     ├─ tagged@0.8.6.1
│     └─ bifunctors@5.5.11
│        ├─ th-abstraction@0.4.3.0
│        ├─ tagged@0.8.6.1
│        ├─ comonad@5.0.8
│        │  ├─ transformers-compat@0.7.1
│        │  ├─ tagged@0.8.6.1
│        │  ├─ indexed-traversable@0.1.2
│        │  └─ distributive@0.6.2.1
│        │     ├─ tagged@0.8.6.1
│        │     └─ base-orphans@0.8.6
│        └─ base-orphans@0.8.6
├─ th-abstraction@0.4.3.0
├─ tagged@0.8.6.1
├─ strict@0.4.0.1
│  ├─ these@1.1.1.1
│  │  ├─ hashable@1.3.5.0
│  │  └─ assoc@1.0.2
│  │     ├─ tagged@0.8.6.1
│  │     └─ bifunctors@5.5.11
│  │        ├─ th-abstraction@0.4.3.0
│  │        ├─ tagged@0.8.6.1
│  │        ├─ comonad@5.0.8
│  │        │  ├─ transformers-compat@0.7.1
│  │        │  ├─ tagged@0.8.6.1
│  │        │  ├─ indexed-traversable@0.1.2
│  │        │  └─ distributive@0.6.2.1
│  │        │     ├─ tagged@0.8.6.1
│  │        │     └─ base-orphans@0.8.6
│  │        └─ base-orphans@0.8.6
│  ├─ hashable@1.3.5.0
│  └─ assoc@1.0.2
│     ├─ tagged@0.8.6.1
│     └─ bifunctors@5.5.11
│        ├─ th-abstraction@0.4.3.0
│        ├─ tagged@0.8.6.1
│        ├─ comonad@5.0.8
│        │  ├─ transformers-compat@0.7.1
│        │  ├─ tagged@0.8.6.1
│        │  ├─ indexed-traversable@0.1.2
│        │  └─ distributive@0.6.2.1
│        │     ├─ tagged@0.8.6.1
│        │     └─ base-orphans@0.8.6
│        └─ base-orphans@0.8.6
├─ scientific@0.3.7.0
│  ├─ primitive@0.7.3.0
│  ├─ integer-logarithms@1.0.3.1
│  └─ hashable@1.3.5.0
├─ primitive@0.7.3.0
├─ hashable@1.3.5.0
├─ dlist@1.0
├─ data-fix@0.3.2
│  └─ hashable@1.3.5.0
├─ base-compat-batteries@0.12.1
│  └─ base-compat@0.12.1
└─ attoparsec@0.14.4
   ├─ scientific@0.3.7.0
   │  ├─ primitive@0.7.3.0
   │  ├─ integer-logarithms@1.0.3.1
   │  └─ hashable@1.3.5.0
   └─ cycle -> attoparsec@0.14.4

----- Project: "/Users/kit/projects/fossa-cli/" (gomod) -----
      73 total deps, 1 direct

github.com/fossas/fossa-cli@v1.1.10
├─ gopkg.in/yaml.v2@v2.2.1
├─ gopkg.in/src-d/go-git.v4@v4.7.1
│  ├─ gopkg.in/warnings.v0@v0.1.2
│  ├─ gopkg.in/src-d/go-git-fixtures.v3@v3.1.1
│  ├─ gopkg.in/check.v1@788fd7840127
│  ├─ golang.org/x/text@v0.3.0
│  ├─ github.com/xanzy/ssh-agent@v0.2.0
│  ├─ github.com/stretchr/testify@v1.2.2
│  ├─ github.com/src-d/gcfg@v1.3.0
│  ├─ github.com/sergi/go-diff@v1.0.0
│  ├─ github.com/pmezard/go-difflib@v1.0.0
│  ├─ github.com/pkg/errors@v0.8.0
│  ├─ github.com/pelletier/go-buffruneio@v0.2.0
│  ├─ github.com/mitchellh/go-homedir@v1.0.0
│  ├─ github.com/kevinburke/ssh_config@81db2a75821e
│  ├─ github.com/jessevdk/go-flags@v1.4.0
│  ├─ github.com/jbenet/go-context@d14ea06fba99
│  ├─ github.com/google/go-cmp@v0.2.0
│  ├─ github.com/gliderlabs/ssh@v0.1.1
│  ├─ github.com/flynn/go-shlex@3f9db97f8568
│  ├─ github.com/davecgh/go-spew@v1.1.1
│  ├─ github.com/anmitsu/go-shlex@648efa622239
│  └─ github.com/alcortesm/tgz@9c5fe88206d7
├─ gopkg.in/src-d/go-billy.v4@v4.3.0
│  ├─ gopkg.in/check.v1@788fd7840127
│  └─ github.com/kr/pretty@v0.1.0
│     └─ github.com/kr/text@v0.1.0
│        └─ github.com/kr/pty@v1.1.1
├─ gopkg.in/ini.v1@v1.62.0
├─ gopkg.in/go-ini/ini.v1@v1.39.1
├─ golang.org/x/sync@112230192c58
├─ github.com/urfave/cli@v1.20.0
├─ github.com/stretchr/testify@v1.2.2
├─ github.com/smartystreets/goconvey@v1.6.4
│  ├─ golang.org/x/tools@ab21143f2384
│  │  └─ golang.org/x/net@d8887717615a
│  │     ├─ golang.org/x/text@v0.3.0
│  │     └─ golang.org/x/crypto@c2843e01d9a2
│  │        └─ golang.org/x/sys@d0b11bdaac8a
│  ├─ github.com/smartystreets/assertions@b2de0cb4f26d
│  ├─ github.com/jtolds/gls@v4.20.0
│  └─ github.com/gopherjs/gopherjs@0766667cb4d1
├─ github.com/rveen/ogdl@9c9b3105ea3e
├─ github.com/rhysd/go-github-selfupdate@v1.0.0
│  ├─ google.golang.org/appengine@v1.2.0
│  │  ├─ golang.org/x/text@v0.3.0
│  │  └─ github.com/golang/protobuf@v1.2.0
│  ├─ golang.org/x/oauth2@d2e6202438be
│  ├─ github.com/ulikunitz/xz@v0.5.4
│  ├─ github.com/tcnksm/go-gitconfig@v0.1.2
│  ├─ github.com/onsi/gomega@v1.4.2
│  │  ├─ gopkg.in/yaml.v2@v2.2.1
│  │  ├─ gopkg.in/tomb.v1@dd632973f1e7
│  │  ├─ gopkg.in/fsnotify.v1@v1.4.7
│  │  ├─ golang.org/x/text@v0.3.0
│  │  ├─ github.com/onsi/ginkgo@v1.6.0
│  │  ├─ github.com/hpcloud/tail@v1.0.0
│  │  ├─ github.com/golang/protobuf@v1.2.0
│  │  └─ github.com/fsnotify/fsnotify@v1.4.7
│  ├─ github.com/inconshreveable/go-update@8152e7eb6ccf
│  ├─ github.com/google/go-querystring@v1.0.0
│  ├─ github.com/google/go-github@v17.0.0
│  └─ github.com/blang/semver@v3.5.1
├─ github.com/remeh/sizedwaitgroup@v1.0.0
├─ github.com/pkg/errors@v0.8.0
├─ github.com/olekukonko/tablewriter@v0.0.2
├─ github.com/mitchellh/mapstructure@v1.0.0
├─ github.com/mitchellh/go-wordwrap@v1.0.0
├─ github.com/mattn/go-runewidth@v0.0.6
├─ github.com/mattn/go-isatty@v0.0.4
├─ github.com/mattn/go-colorable@v0.0.9
├─ github.com/gnewton/jargo@41f5f186a805
├─ github.com/fatih/color@v1.7.0
├─ github.com/emirpasic/gods@v1.12.0
├─ github.com/cheekybits/genny@v1.0.0
├─ github.com/briandowns/spinner@9f016caa1359
├─ github.com/bmatcuk/doublestar@v1.1.1
├─ github.com/blang/semver@v3.5.1
├─ github.com/apex/log@v1.0.0
├─ github.com/Masterminds/semver@v1.5.0
└─ github.com/BurntSushi/toml@v0.3.1
```

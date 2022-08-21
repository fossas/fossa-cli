# Strategies

<!-- add table here

- Analyzers
  - By language
  - By platform
    - Might have duplicates
      - Carthage is both "iOS" as platform and "Objective-C" as language
      - .NET is platform, C# is language
      - Conda is platform, Python is language
    - TODO: add scripting/linting to let us say "file at this folder location is the same as other file" and check that their contents are identical (e.g. so we can duplicate Carthage file under both iOS and Objective-C)
  - System deps
  - Others
    - Docker
-->

## Supported Languages

### clojure

- [leiningen](languages/clojure/leiningen.md)

### dart

- [pub](languages/dart/pub.md)

### erlang

- [rebar3](languages/erlang/erlang.md)

### elixir

- [mix](languages/elixir/mix.md)

### fortran

- [fortran](languages/fortran/fortran.md)

### golang

- [gomodules (`go mod`)](languages/golang/gomodules.md)
- [dep](languages/golang/godep.md)
- [glide](languages/golang/glide.md)

### haskell

- [cabal](languages/haskell/cabal.md)
- [stack](languages/haskell/stack.md)

### java

- [maven](languages/maven/maven.md)
- [gradle](languages/gradle/gradle.md)

### javascript/typescript

- [yarn](languages/nodejs/yarn.md)
- [npm](languages/nodejs/npm.md)
- [pnpm](languages/nodejs/pnpm.md)
### nim

- [Nimble](languages/nim/nimble.md)

### .NET

- [NuGet](languages/dotnet/nuget.md)
- [Paket](languages/dotnet/paket.md)

### objective-c

- [carthage](platforms/ios/carthage.md)
- [cocoapods](platforms/ios/cocoapods.md)

### perl

- [perl](languages/perl/perl.md)

### php

- [php](languages/php/composer.md)

### python

- [conda](platforms/conda/conda.md)
- [`requirements.txt`/`setup.py`](languages/python/python.md)
- [pipenv](languages/python/pipenv.md)
- [poetry](languages/pythong/[../python/poetry.md])

### ruby

- [bundler](languages/ruby/bundler.md)

### rust

- [cargo](languages/rust/cargo.md)

### scala

- [sbt](languages/scala/sbt.md)
- [gradle](languages/gradle/gradle.md)
- [maven](languages/maven/maven.md)

### swift

- [carthage](platforms/ios/carthage.md)
- [cocoapods](platforms/ios/cocoapods.md)
- [swiftPM](platforms/ios/swift.md)

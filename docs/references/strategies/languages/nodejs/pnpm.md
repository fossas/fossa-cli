# Pnpm

[Pnpm](https://pnpm.io/) is a fast, disk space-efficient package manager. 
Unlike npm and yarn, pnpm uses symbolic links to create a nested structure
of dependencies.

## Project Discovery

Find files named `pnpm-lock.yaml` with a corresponding `package.json` file.

## Analysis

Only `pnpm-lock.yaml` is used for analysis. CLI will parse and use the following fields.
in `pnpm-lock.yaml` to analyze the dependency graph.

- `importers`
  - `[importersKey]`
    - `dependencies`: list of direct dependencies
    - `devDependencies`: list of development dependencies

- `packages`
  - `[packagesKey]`
    - `resolution`: infer git URL, git commit, or package source URL.  
    - `dependencies`: list of transitive dependencies 
    - `peerDependencies`: list of peer dependencies (will be treated like any other dependency)
    - `dev`: to infer if this is used dependency or not. If the value is `true` by default CLI will not include this in the final analysis.

An example is provided below:

```yml
lockfileVersion: 5.4

importers:
  .:
    specifiers:
      some-pkg: https://some-url/pkg.tar.gz
      react: '*'
      my-local-pkg: file:../libs/my-local-pkg 
    dependencies:
      some-pkg: '@some-url/pkg.tar.gz'
      my-local-pkg: file:../libs/my-local-pkg
    devDependencies:
      react: 18.1.0

  # workspace project in packages/some-ws-pkg directory from root.
  packages/some-ws-pkg: 
    specifiers:
      commander: 9.2.0
    dependencies:
      commander: 9.2.0

packages:
    '@some-url/pkg.tar.gz':
        resolution: {tarball: https://some-url/pkg.tar.gz}
        name: some-pkg
        version: 1.0.0
        engines: {node: '>=4.0.0'}
        dev: false

    file:../libs/my-local-pkg:
        resolution: {directory: "../libs/my-local-pkg", type: directory}
        name: unifier
        version: 1.0.0
        dependencies:
            loose-envify: 1.4.0
        engines: {node: '>=4.0.0'}
        dev: false

    /commander/9.2.0:
        resolution: {integrity: sha512-e2i4wANQiSXgnrBlIatyHtP1odfUp0BbV5Y5nEGbxtIrStkEOAAzCUirvLBNXHLr7kwLvJl6V+4V3XV9x7Wd9w==}
        engines: {node: ^12.20.0 || >=14}
        dev: false

    /react/18.1.0:
        resolution: {integrity: sha512-4oL8ivCz5ZEPyclFQXaNksK3adutVS8l2xzZU0cqEFrE9Sb7fC0EFK5uEk74wIreL1DERyjvsU915j1pcT2uEQ==}
        engines: {node: '>=0.10.0'}
        dependencies:
            loose-envify: 1.4.0
        dev: true

    /loose-envify/1.4.0:
        resolution: {integrity: sha512-lyuxPGr/Wfhrlem2CL/UcnUc1zcqKAImBDzukY7Y5F/yQiNdko6+fRLevlw1HgMySw7f611UIY408EtxRSoK3Q==}
        hasBin: true
        dependencies:
            js-tokens: 4.0.0
        dev: false

    /js-tokens/4.0.0:
        resolution: {integrity: sha512-RdJUflcE3cUzKiMqQgsCu06FPu9UdIJO0beYbPhHN4k6apgJtifcoCtT9bcxOpYBtpD2kCM6Sbzg4CausW/PKQ==}
        dev: true

```

* If the dependency was resolved using git (`resolution` will have `type: git` attribute),
FOSSA will use provided `repo` and `commit` attribute to analyze this dependency.

```yaml
    # FOSSA will use `commit` and `repo` to analyze the this dependency.
    github.com/Marak/colors.js/6bc50e79eeaa1d87369bb3e7e608ebed18c5cf26:
      resolution: {commit: 6bc50e79eeaa1d87369bb3e7e608ebed18c5cf26, repo: git+ssh://git@github.com/Marak/colors.js, type: git}
      name: colors
      version: 1.4.44-liberty-2
      engines: {node: '>=0.1.90'}
      dev: false
```

* If the dependency was resolved using tarball (`resolution` will have `tarball` attribute) 
FOSSA will use provided URL address to download and analyze this dependency.

```yaml
    # FOSSA will analyze lodash from the tarball URL.
    '@some-url/pkg.tar.gz':
        resolution: {tarball: https://some-url/pkg.tar.gz}
        name: some-pkg
        version: 1.0.0
        engines: {node: '>=4.0.0'}
        dev: false
```

* If the dependency was resolved using the local directory (`resolution` will have the `type: directory` attribute),
FOSSA will not analyze this dependency. Local dependency's transitive dependencies will be analyzed, 
and they will be promoted in place of local dependency. 

```yaml
    # FOSSA will not analyze this dependency, 
    # But FOSSA will analyze its transitive dependency (if they are not sourced from the local directory)
    #
    # FOSSA will promote loose-envify of 1.4.0 in place of unifier.
    file:../libs/my-local-pkg:
        resolution: {directory: "../libs/my-local-pkg", type: directory}
        name: unifier
        version: 1.0.0
        dependencies:
            loose-envify: 1.4.0
        engines: {node: '>=4.0.0'}
        dev: false
```

* If the dependency was resolved using registry resolver, FOSSA will use the registry to analyze the dependency. 
CLI will infer the package name and version using `/${dependencyName}/${dependencyVersion}` scheme from the package's key.

```yaml
    # Resolves to npm dependency: commander with 9.2.0 version
    /commander/9.2.0:
        resolution: {integrity: sha512-e2i4wANQiSXgnrBlIatyHtP1odfUp0BbV5Y5nEGbxtIrStkEOAAzCUirvLBNXHLr7kwLvJl6V+4V3XV9x7Wd9w==}
        engines: {node: ^12.20.0 || >=14}
        dev: false
```

* Peer dependencies will be included in the analysis (they are treated like any other dependency).
* Pnpm workspaces are supported.
* Development dependencies (`dev: true`) are ignored by default from analysis. To include them in the analysis, execute CLI with `--include-unused` flag e.g. `fossa analyze --include-unused`.
* Optional dependencies are included in the analysis by default. They can be ignored in FOSSA UI.
* `fossa-cli` supports lockFileVersion: 4.x, 5.x, and 6.x.


# F.A.Q

### How do I perform analysis only for pnpm projects?

You can explicitly specify an analysis target in `.fossa.yml` file. The example below will exclude all analysis targets except for pnpm.

```yaml
# .fossa.yml

version: 3
targets:
  only:
    - type: pnpm
```
### Are all versions of `pnpm` supported?

At this time, the latest version of pnpm (v9) and its associated v9 lockfiles are not correctly parsed by FOSSA. Please revert to v8 (v6 lockfile) if your dependencies are not resolved in the FOSSA UI: "FOSSA was unable to analyze this dependency. If it is behind a private registry or auth you may need to configure FOSSA's access, then rebuild this dependency." This is due to the version number being appended to the package name:

<img width="796" alt="image" src="https://github.com/user-attachments/assets/d1461506-d3e7-42da-b9be-2b53a87f79f1" />

Please [email](mailto:support@fossa.com) FOSSA support if you are affected by this limitation.

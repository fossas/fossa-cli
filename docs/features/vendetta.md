
# Vendetta

Vendetta is the name of FOSSA's vendored dependency identification feature.

Vendetta hashes files in your first party source code, compares them against
FOSSA's knowledge base, and matches them to common open source components before
finally feeding those matches to a special algorithm that deduces a holistic set
of vendored open source dependencies present in your project.

Vendetta can be run as part of `fossa analyze`. To enable it, add the
`--x-vendetta` flag when you run `fossa analyze`:

```sh
fossa analyze --x-vendetta
```

## How Vendetta Works

When `--x-vendetta` is enabled, the CLI:

1. **Hashes Files**: Creates MD5 hashes of the contents of all relevant files.
2. **Filters Content**: By default, skips directories like `.git/`, and hidden
    directories. This includes, from `.fossa.yml`,
    `vendoredDependencies.licenseScanPathFilters.exclude`, documented further
    below.
5. **Uploads Hashes**: Sends only the hashes to FOSSA's servers.
6. **Receives Matches**: Gets back information about any matching open source
   components.
7. **Infers Dependencies**: Feeds the matches to an algorithm that heuristically
   identifies the vendored dependencies in your project.

## Data Sent to FOSSA

Vendetta sends _only_ the MD5 hashes of your file contents to FOSSA. The raw
contents are never sent to FOSSA.

## Data Retention

The MD5 hashes are stored permanently in FOSSA.

## Directory Filtering

By default, Vendetta excludes common non-production directories and follows
`.gitignore` patterns:

- Hidden directories.
- Globs as directed by `.gitignore` files.

#### Custom Exclude Filtering

You can customize which files and directories are excluded from Vendetta by
configuring exclude filters in your `.fossa.yml` file. Note that Vendetta scans
currently only support exclude patterns, not `only` patterns.

For example:
```yaml
version: 3
vendoredDependencies:
  licenseScanPathFilters:
    exclude:
      - "**/test/**"
      - "**/tests/**"
      - "**/spec/**"
      - "**/node_modules/**"
      - "**/dist/**"
      - "**/build/**"
      - "**/*.test.js"
      - "**/*.spec.ts"
```

**Important Notes:**

- Vendetta scanning only use the `exclude` filters from `licenseScanPathFilters`
  — `only` filters are ignored for this use-case.
- Path filters use standard glob patterns (e.g., `**/*` for recursive matching,
  `*` for single-directory matching).
- The configuration goes in the
  `vendoredDependencies.licenseScanPathFilters.exclude` section.
- These exclude patterns are passed directly to the Ficus scanning engine as
  `--exclude` arguments.
- Default exclusions (hidden files, `.gitignore` patterns) are applied in
  addition to custom excludes.

## A note on scan times

The first time you run Vendetta on a codebase, it may take a long time to scan.
For example, scanning [Linux](https://github.com/torvalds/linux) for the first
time may take upwards of 60 minutes. This is because most of the files in your
codebase will have never been checked against FOSSA's knowledge base for open
source components, which can take time.

Once you scan the first time however, FOSSA will cache the open source component
matches for each MD5 hash Vendetta provides. This means that subsequent scans of
the same project will be drastically faster. For example, scanning the same
revision of Linux twice in a row should result in the second scan taking only
1-2 minutes.

The time it takes to scan newer versions of your codebase will depend on how
many files in the new version have not been previously scanned. A file has been
previously scanned if the exact same file has ever been scanned by Vendetta.
FOSSA recommends scanning your codebase on a regular basis to keep scan times
low. Additionally, if you intend on running Vendetta as part of your CI
pipeline, it might be best to do a manual run first on a local machine. That
way, future automated scans of your project will be able to benefit from the
initial caching done in the first scan.

## Invalid certificate errors

You may encounter an error like:

```
error uploading digest batch:
   0: upload digests
   1: error sending request for url (https://app.fossa.com/api/proxy/analysis/api/x/snippets/digests)
   2: client error (Connect)
   3: invalid peer certificate: UnknownIssuer
```

This error occurs when corporate network infrastructure (such as a TLS inspection
proxy or firewall) intercepts HTTPS traffic and presents its own certificate.
The CLI cannot verify this certificate because the signing authority is not in
the default trust store.

To resolve this, set the `ALLOW_INVALID_CERTS` environment variable:

```sh
ALLOW_INVALID_CERTS=1 fossa analyze --x-vendetta
```

This instructs the CLI to accept certificates it cannot verify.

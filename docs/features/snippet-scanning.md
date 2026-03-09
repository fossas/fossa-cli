
# Snippet Scanning

Snippet scanning identifies potential open source code snippets within your first-party source code by comparing file fingerprints against FOSSA's knowledge base. This feature helps detect code that may have been copied from open source projects.

Snippet Scanning runs as part of `fossa analyze`. To enable it, add the `--snippet-scan` flag when you run `fossa analyze`:

```
fossa analyze --snippet-scan
```

Snippet Scanning must also be enabled for your organization, and is only available for enterprise customers. If you would like to enable it for your organization, please [contact us](https://support.fossa.com).

## How Snippet Scanning Works

When `--snippet-scan` is enabled, the CLI:

1. **Hashes Files First**: Creates CRC64 hashes of all source files to identify which files need fingerprinting
2. **Checks Necessity of Fingerprinting**: Checks with FOSSA servers to determine which file hashes are already known
3. **Fingerprints New or Changed Files**: Uses the Ficus fingerprinting engine to create cryptographic fingerprints only for files not previously seen
4. **Filters Content**: By default, skips directories like `.git/`, and hidden directories. This includes, from `.fossa.yml`, `vendoredDependencies.licenseScanPathFilters.exclude`, documented further below.
5. **Uploads Fingerprints**: Sends only the fingerprints to FOSSA's servers
6. **Receives Matches**: Gets back information about any matching open source components
7. **Uploads Match Contents**: For files that have matches, uploads source code content temporarily to FOSSA servers (see below for more details on what we send and how long it is retained for).

## Data Sent to FOSSA

**For Performance Optimization:**
- CRC64 hashes of all files, to avoid re-fingerprinting unchanged files.

**For Fingerprinting:**
- Fingerprints of source code to identify matches.

**For Matched Files Only:**
- The full content of all files that contain snippet matches.

## Data Retention

- **File Fingerprints**: Stored permanently for caching and performance optimization
- **Source Code Content**: Stored temporarily for 30 days and then automatically deleted
- **CRC64 Hashes**: The likelihood of a collision with CRC64 (2^64 possible values) is extremely low.

## Directory Filtering

By default, snippet scanning excludes common non-production directories and follows `.gitignore` patterns:

- Hidden directories.
- Globs as directed by `.gitignore` files.

#### Custom Exclude Filtering

You can customize which files and directories are excluded from snippet scanning by configuring exclude filters in your `.fossa.yml` file. Note that snippet scanning currently only supports exclude patterns, not `only` patterns.

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

- Snippet scanning only uses the `exclude` filters from `licenseScanPathFilters` - `only` filters are ignored for this use-case.
- Path filters use standard glob patterns (e.g., `**/*` for recursive matching, `*` for single-directory matching).
- The configuration goes in the `vendoredDependencies.licenseScanPathFilters.exclude` section.
- These exclude patterns are passed directly to the Ficus fingerprinting engine as `--exclude` arguments.
- Default exclusions (hidden files, `.gitignore` patterns) are applied in addition to custom excludes.

## A note on scan times

The first time you run a snippet scan on a codebase, it may take a long time to scan. For example, scanning [Linux](https://github.com/torvalds/linux) for the first time takes around 60 minutes. This is because most of the files in your codebase will not exist in FOSSA's knowledge base, and we will need to fingerprint and compare all of them to our snippet scan corpus.

However, the next time you scan that codebase we will only need to re-fingerprint and compare files that have changed since the previous scan, and the scan will be much faster. For example, if you snippet scan that same revision of Linux a second time, the scan will complete in less than a minute.

Because of this speed difference, we recommend doing a manual scan of your project before enabling Snippet Scanning in CI. This will avoid running multiple slow scans, as any scans started before the first scan completes will also be slower.

The time it takes to scan newer versions of your codebase will depend on how many files in the new version have not been previously scanned. A file has been previously scanned if the exact same file has ever been snippet scanned. FOSSA recommends snippet scanning your codebase on a regular basis to keep scan times low.

## The Snippet Scan Summary
<!-- Note: this section is linked to from the snippet scan summary in src/App/Fossa/Ficus/Analyze.hs. So if you change this heading name or the path
to this file, you will need to update the link there as well -->

When a Snippet Scan completes, the CLI will output a summary of the scan. It will look like this:

```
 ============================================================
  Snippet scan summary:
    Analysis ID: 110054
    Bucket ID: 110551
    Files skipped: 6
    Total Files processed: 18
    Unique Files processed: 13
    Unique Files with matches found: 4
    Unique Files with no matches found: 9
    Unique Files already in our knowledge base: 11
    Unique Files new to our knowledge base: 2
    Processing time: 0.087s
  ============================================================
```

Here is a description of what each line means:

<dl>
<dt>Analysis ID</dt>
<dd>The ID of the Snippet Scan analysis stored in FOSSA's servers. This is used by FOSSA's support team.</dd>

<dt>Bucket ID</dt>
<dd>The ID of the temporary storage bucket where we store Snippet Scan results before processing them. This is used by FOSSA's support team.</dd>

<dt>Files Skipped</dt>
<dd>The number of files skipped during the Snippet Scan.</dd>

<dt>Total Files Processed</dt>
<dd>The number of files processed during the Snippet Scan. This count includes every processed file, even if the same file contents are included multiple times.</dd>

<dt>Unique Files processed</dt>
<dd>The number of unique files processed during the Snippet Scan. If we scan multiple files with the same contents, they will only be counted once.</dd>

<dt>Unique Files with matches found</dt>
<dd>The number of unique files where we found a potential match to Open Source code</dd>

<dt>Unique files with no matches found</dt>
<dd>The number of unique files where no potential matches to Open Source code were found.</dd>

<dt>Unique Files already in our knowledge base</dt>
<dd>The number of files that already exist in FOSSA's knowledge base. These files do not need to be fingerprinted.</dd>

<dt>Unique Files new to our knowledge base</dt>
<dd>The number of files that do not exist in FOSSA's knowledge base. These files needed to be fingerprinted in this Snippet Scan.</dd>
</dl>

## Invalid certificate errors

You may encounter an error like:

```
error uploading digest batch:
   0: upload digests
   1: error sending request for url (https://app.fossa.com/api/proxy/analysis/api/x/snippets/digests)
   2: client error (Connect)
   3: invalid peer certificate: UnknownIssuer
```

This can occur in environments that use custom TLS certificates. To resolve this,
set the `ALLOW_INVALID_CERTS` environment variable:

```sh
ALLOW_INVALID_CERTS=1 fossa analyze --snippet-scan
```

This instructs the CLI to accept certificates it cannot verify.

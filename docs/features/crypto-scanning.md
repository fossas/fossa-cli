
# Crypto Scanning

Crypto Scanning is the name of FOSSA's cryptographic algorithm detection feature.

Crypto Scanning analyzes source code, dependency manifests, and configuration files
in your project, identifies cryptographic algorithm usage, and classifies each
finding against FIPS 140-3 compliance requirements. Results can be uploaded to
FOSSA, exported as a CycloneDX 1.7 CBOM (Cryptography Bill of Materials), or
printed as a FIPS compliance report.

Crypto Scanning can be run as part of `fossa analyze`. To enable it, add the
`--x-crypto-scan` flag when you run `fossa analyze`:

```sh
fossa analyze --x-crypto-scan
```

## How Crypto Scanning Works

When `--x-crypto-scan` is enabled, the CLI:

1. **Detects Ecosystems**: Identifies which language ecosystems are present in your
   project (e.g., Python, Java, Go, Rust, Node.js, Ruby, C#/.NET, PHP, Swift, Elixir).
2. **Scans Source Files**: Uses pattern-based detection across four categories:
   - **Dependency analysis**: Known crypto libraries in dependency manifests
     (e.g., `pyca/cryptography` in `requirements.txt`, `ring` in `Cargo.toml`)
   - **Import pattern matching**: Crypto-related imports
     (e.g., `import javax.crypto.Cipher`, `from cryptography.hazmat.primitives import hashes`)
   - **API call pattern matching**: Crypto API invocations
     (e.g., `Cipher.getInstance("AES/GCM/NoPadding")`, `hashlib.sha256()`)
   - **Configuration file scanning**: TLS configs, OpenSSL configs, security properties
3. **Classifies Algorithms**: Maps each detected algorithm to its FIPS 140-3 status
   (approved, deprecated, or not approved) and assesses key sizes against NIST minimums.
4. **Produces Results**: Outputs findings as part of the standard analysis pipeline,
   with optional CycloneDX CBOM export and FIPS compliance reporting.

## Supported Ecosystems

| Ecosystem | Crypto Libraries Detected | File Types Scanned |
|---|---|---|
| **Python** | cryptography, pycryptodome, hashlib, ssl | `*.py`, `requirements.txt`, `pyproject.toml` |
| **Java** | JCA/JCE, BouncyCastle, Conscrypt | `*.java`, `*.kt`, `pom.xml`, `build.gradle` |
| **Go** | crypto/*, x/crypto | `*.go`, `go.mod` |
| **Rust** | ring, rust-crypto, openssl, rustls | `*.rs`, `Cargo.toml` |
| **Node.js** | crypto (builtin), crypto-js, node-forge, jose | `*.js`, `*.ts`, `package.json` |
| **Ruby** | OpenSSL, rbnacl, bcrypt-ruby | `*.rb`, `Gemfile`, `*.gemspec` |
| **C#/.NET** | System.Security.Cryptography, BouncyCastle | `*.cs`, `*.csproj`, `packages.config` |
| **PHP** | openssl/sodium extensions, phpseclib | `*.php`, `composer.json` |
| **Swift** | CryptoKit, CommonCrypto | `*.swift`, `Package.swift`, `Podfile` |
| **Elixir** | :crypto, Comeonin (bcrypt/argon2), JOSE | `*.ex`, `*.exs`, `mix.exs` |

## Data Sent to FOSSA

When crypto scan results are uploaded to FOSSA (the default behavior without `--output`),
the following data is sent:

- Algorithm names and classifications (e.g., "AES-256-GCM", "SHA-256")
- File paths where algorithms were detected
- Detection confidence levels
- FIPS compliance status per algorithm
- Providing library names (e.g., "openssl", "ring")

No source code content is sent to FOSSA. Only metadata about detected
cryptographic algorithm usage is transmitted.

## CycloneDX 1.7 CBOM Output

To export a local CycloneDX 1.7 CBOM file instead of (or in addition to)
uploading to FOSSA, use the `--crypto-cbom-output` flag:

```sh
fossa analyze --crypto-cbom-output /path/to/cbom.json
```

This produces a standards-compliant CycloneDX 1.7 JSON file with:

- `cryptographic-asset` component types
- `cryptoProperties` with `algorithmProperties` (primitive, mode, key size, FIPS level)
- `provides` dependency relationships linking libraries to their algorithms
- Algorithm OIDs where applicable

The `--crypto-cbom-output` flag implies `--x-crypto-scan` and does not need to
be combined with it explicitly.

## FIPS Compliance Report

To print a FIPS compliance summary to stdout, use the `--crypto-fips-report` flag:

```sh
fossa analyze --crypto-fips-report
```

The report includes:

- **Summary statistics**: Total algorithms detected, FIPS-approved count,
  deprecated count, non-FIPS count, and overall compliance percentage
- **Per-algorithm breakdown**: Each detected algorithm with its FIPS status
- **Remediation suggestions**: For non-FIPS algorithms, recommended FIPS
  alternatives (e.g., "Replace ChaCha20-Poly1305 with AES-256-GCM")
- **Key size warnings**: Flags algorithms with key sizes below NIST minimums

The `--crypto-fips-report` flag implies `--x-crypto-scan` and does not need to
be combined with it explicitly.

### Example output

```
FIPS Compliance Report
======================

Summary: 23 algorithms detected
  Approved:      15 (65%)
  Deprecated:     3 (13%)
  Not Approved:   5 (22%)

Remediation Suggestions:
  ChaCha20-Poly1305  ->  AES-256-GCM
  BLAKE2b            ->  SHA-256 / SHA-3
  X25519             ->  ECDH P-256
  bcrypt             ->  PBKDF2
  MD5                ->  SHA-256
```

## Combining Flags

All crypto scanning flags can be combined:

```sh
# Scan, upload results, export CBOM, and print FIPS report
fossa analyze --x-crypto-scan --crypto-cbom-output cbom.json --crypto-fips-report

# Local-only: export CBOM without uploading
fossa analyze --output --crypto-cbom-output cbom.json

# FIPS report only
fossa analyze --output --crypto-fips-report
```

## FIPS Compliance Reference

### FIPS-Approved Algorithms

| Category | Algorithms |
|---|---|
| Symmetric Encryption | AES-128/192/256 (all modes except ECB deprecated by 2030) |
| Hash Functions | SHA-256, SHA-384, SHA-512, SHA-3 family, SHAKE128/256 |
| Signatures | RSA >= 2048-bit, ECDSA (P-256/P-384/P-521), EdDSA, ML-DSA |
| Key Exchange | ECDH (P-256/P-384/P-521), DH >= 2048-bit, ML-KEM |
| MACs | HMAC, CMAC, GMAC, KMAC |
| KDFs | HKDF, PBKDF2, SP 800-108 KDFs |

### Common Non-FIPS Algorithms

| Algorithm Found | Recommended FIPS Alternative |
|---|---|
| ChaCha20-Poly1305 | AES-256-GCM |
| BLAKE2/BLAKE3 | SHA-256 / SHA-3 |
| X25519/X448 | ECDH P-256/P-384 |
| MD5 | SHA-256 |
| RC4, Blowfish, DES | AES |
| Argon2, scrypt, bcrypt | PBKDF2 |

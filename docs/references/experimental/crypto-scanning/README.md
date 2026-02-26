# Crypto Scanning

FOSSA supports the ability to detect cryptographic algorithm usage in your project source tree and assess FIPS 140-3 compliance via an opt-in flag (`--x-crypto-scan`).

The core idea behind this feature is that organizations subject to FIPS compliance requirements need visibility into which cryptographic algorithms their software uses, whether those algorithms are FIPS-approved, and what remediation steps are needed for non-compliant usage.

_Important: For support and other general information, refer to the [experimental options overview](../README.md) before using experimental options._

## Discovery

Crypto Scanning automatically detects which language ecosystems are present in your project by examining manifest files (e.g., `requirements.txt`, `pom.xml`, `go.mod`, `Cargo.toml`, `package.json`, `Gemfile`, `*.csproj`, `composer.json`, `Package.swift`, `mix.exs`).

Ten ecosystems are supported: Python, Java, Go, Rust, Node.js, Ruby, C#/.NET, PHP, Swift, and Elixir.

## Analysis

The scanner uses four detection methods, applied in order of specificity:

| Detection Method | Description | Example |
|---|---|---|
| Dependency manifest | Known crypto libraries in lock/manifest files | `cryptography` in `requirements.txt` |
| Import statement | Crypto-related import/require patterns | `import "crypto/aes"` in Go |
| API call | Direct crypto API invocations | `Cipher.getInstance("AES/GCM/NoPadding")` in Java |
| Configuration file | TLS/SSL/crypto configuration entries | `ssl_protocols TLSv1.3` in nginx config |

Each detected algorithm is classified with:

- **FIPS status**: Approved, deprecated, or not approved per NIST SP 800-131A Rev. 2
- **Key size assessment**: Whether the key size meets NIST minimum requirements
- **Confidence level**: High, medium, or low based on detection method specificity
- **Providing library**: The library or framework providing the algorithm

## Output Formats

| Flag | Output |
|---|---|
| `--x-crypto-scan` | Include crypto findings in standard FOSSA upload |
| `--crypto-cbom-output FILE` | Write CycloneDX 1.7 CBOM JSON to a local file |
| `--crypto-fips-report` | Print FIPS compliance summary to stdout |

Both `--crypto-cbom-output` and `--crypto-fips-report` imply `--x-crypto-scan`.

## More Detail

For the full list of supported ecosystems, detected libraries, FIPS compliance reference, and usage examples, see [the Crypto Scanning feature documentation](../../../features/crypto-scanning.md).

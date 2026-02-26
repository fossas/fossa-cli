use std::collections::HashMap;
use std::fs;
use std::path::Path;

use walkdir::WalkDir;

use crate::crypto_algorithm::{Confidence, CryptoAlgorithm, CryptoFinding, Primitive};
use crate::fips;
use crate::patterns::{self, CryptoPattern};

/// Auto-detect ecosystems present in a project directory.
pub fn detect_ecosystems(project_path: &Path) -> Vec<String> {
    let mut ecosystems = Vec::new();

    for entry in WalkDir::new(project_path)
        .max_depth(3)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let name = entry.file_name().to_string_lossy();
        match name.as_ref() {
            "requirements.txt" | "Pipfile" | "pyproject.toml" | "setup.py" | "setup.cfg" => {
                if !ecosystems.contains(&"python".to_string()) {
                    ecosystems.push("python".to_string());
                }
            }
            "pom.xml" | "build.gradle" | "build.gradle.kts" => {
                if !ecosystems.contains(&"java".to_string()) {
                    ecosystems.push("java".to_string());
                }
            }
            "go.mod" | "go.sum" => {
                if !ecosystems.contains(&"go".to_string()) {
                    ecosystems.push("go".to_string());
                }
            }
            "package.json" => {
                if !ecosystems.contains(&"node".to_string()) {
                    ecosystems.push("node".to_string());
                }
            }
            "Cargo.toml" => {
                if !ecosystems.contains(&"rust".to_string()) {
                    ecosystems.push("rust".to_string());
                }
            }
            "Gemfile" | "Rakefile" => {
                if !ecosystems.contains(&"ruby".to_string()) {
                    ecosystems.push("ruby".to_string());
                }
            }
            "composer.json" => {
                if !ecosystems.contains(&"php".to_string()) {
                    ecosystems.push("php".to_string());
                }
            }
            "Package.swift" | "Podfile" => {
                if !ecosystems.contains(&"swift".to_string()) {
                    ecosystems.push("swift".to_string());
                }
            }
            "mix.exs" => {
                if !ecosystems.contains(&"elixir".to_string()) {
                    ecosystems.push("elixir".to_string());
                }
            }
            _ => {
                // Detect C#/.NET by file extension
                let ext = entry.path().extension().and_then(|e| e.to_str()).unwrap_or("");
                if (ext == "csproj" || ext == "fsproj" || ext == "sln")
                    && !ecosystems.contains(&"csharp".to_string())
                {
                    ecosystems.push("csharp".to_string());
                }
                // Detect Ruby by .gemspec extension
                if ext == "gemspec" && !ecosystems.contains(&"ruby".to_string()) {
                    ecosystems.push("ruby".to_string());
                }
            }
        }
    }

    ecosystems
}

/// Scan a project for cryptographic algorithm usage.
pub fn scan_project(project_path: &Path, ecosystems: &[String]) -> Vec<CryptoFinding> {
    let all_patterns = patterns::build_patterns();
    let mut findings = Vec::new();

    for entry in WalkDir::new(project_path)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
    {
        let path = entry.path();

        // Skip hidden directories and common non-source dirs
        if should_skip_path(path) {
            continue;
        }

        let extension = path
            .extension()
            .and_then(|e| e.to_str())
            .unwrap_or("");

        let file_name = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("");

        // Read file content
        let content = match fs::read_to_string(path) {
            Ok(c) => c,
            Err(_) => continue, // Skip binary files
        };

        let rel_path = path
            .strip_prefix(project_path)
            .unwrap_or(path)
            .to_string_lossy()
            .to_string();

        // Match against all applicable patterns
        for pattern in &all_patterns {
            // Check if this pattern applies to the current ecosystem and file extension
            if !pattern_applies(pattern, ecosystems, extension, file_name) {
                continue;
            }

            for mat in pattern.regex.find_iter(&content) {
                let line_number = content[..mat.start()]
                    .chars()
                    .filter(|&c| c == '\n')
                    .count()
                    + 1;

                let algorithm = resolve_algorithm(&pattern.algorithm_name, mat.as_str());

                findings.push(CryptoFinding {
                    algorithm,
                    file_path: rel_path.clone(),
                    line_number,
                    matched_text: mat.as_str().to_string(),
                    detection_method: pattern.detection_method.clone(),
                    ecosystem: pattern.ecosystem.to_string(),
                    providing_library: pattern.providing_library.clone(),
                    confidence: pattern.confidence.clone(),
                });
            }
        }
    }

    // Deduplicate findings with same algorithm in same file (keep highest confidence)
    deduplicate_findings(findings)
}

fn should_skip_path(path: &Path) -> bool {
    let path_str = path.to_string_lossy();
    let skip_dirs = [
        "/.git/",
        "/node_modules/",
        "/__pycache__/",
        "/.venv/",
        "/venv/",
        "/target/",
        "/dist/",
        "/build/",
        "/.tox/",
        "/.mypy_cache/",
        "/.pytest_cache/",
        "/vendor/",
        "/.gradle/",
        "/.idea/",
        "/.vscode/",
    ];
    skip_dirs.iter().any(|d| path_str.contains(d))
}

fn pattern_applies(pattern: &CryptoPattern, ecosystems: &[String], file_ext: &str, file_name: &str) -> bool {
    // Check ecosystem match
    let ecosystem_match = pattern.ecosystem == "any"
        || ecosystems.iter().any(|e| {
            e == pattern.ecosystem
                || (e == "node" && (pattern.ecosystem == "javascript" || pattern.ecosystem == "typescript"))
        });

    if !ecosystem_match {
        return false;
    }

    // Check file extension match
    if pattern.file_extensions.iter().any(|ext| *ext == file_ext) {
        return true;
    }

    // For extensionless files (Gemfile, Podfile, Pipfile, etc.), check manifest names
    if file_ext.is_empty() {
        return patterns::ecosystem_manifests(pattern.ecosystem)
            .iter()
            .any(|m| !m.contains('*') && *m == file_name);
    }

    false
}

fn resolve_algorithm(name: &str, _matched_text: &str) -> CryptoAlgorithm {
    let (fips_status, _remediation) = fips::classify_algorithm(name);

    let (primitive, family, mode, param_set, curve, security, quantum, oid, functions) = match name {
        // Symmetric - AES
        "AES" | "AES-128" => (Primitive::BlockCipher, "AES", None, Some("128"), None, Some(128), 1, Some("2.16.840.1.101.3.4.1"), vec!["keygen", "encrypt", "decrypt"]),
        "AES-256" => (Primitive::BlockCipher, "AES", None, Some("256"), None, Some(256), 1, Some("2.16.840.1.101.3.4.1"), vec!["keygen", "encrypt", "decrypt"]),
        "AES-GCM" | "AES-256-GCM" => (Primitive::Ae, "AES", Some("gcm"), Some("256"), None, Some(256), 1, Some("2.16.840.1.101.3.4.1.46"), vec!["keygen", "encrypt", "decrypt", "tag"]),
        "AES-128-GCM" => (Primitive::Ae, "AES", Some("gcm"), Some("128"), None, Some(128), 1, Some("2.16.840.1.101.3.4.1.6"), vec!["keygen", "encrypt", "decrypt", "tag"]),
        "AES-CBC" | "AES-256-CBC" => (Primitive::BlockCipher, "AES", Some("cbc"), Some("256"), None, Some(256), 1, None, vec!["keygen", "encrypt", "decrypt"]),
        "AES-128-CBC" => (Primitive::BlockCipher, "AES", Some("cbc"), Some("128"), None, Some(128), 1, None, vec!["keygen", "encrypt", "decrypt"]),
        "AES-CTR" => (Primitive::BlockCipher, "AES", Some("ctr"), None, None, Some(128), 1, None, vec!["keygen", "encrypt", "decrypt"]),
        "AES-ECB" => (Primitive::BlockCipher, "AES", Some("ecb"), None, None, Some(128), 1, None, vec!["keygen", "encrypt", "decrypt"]),

        // Symmetric - non-AES
        "ChaCha20-Poly1305" | "ChaCha20" => (Primitive::Ae, "ChaCha20-Poly1305", None, Some("256"), None, Some(256), 0, None, vec!["keygen", "encrypt", "decrypt", "tag"]),
        "3DES" => (Primitive::BlockCipher, "3DES", Some("cbc"), Some("168"), None, Some(112), 0, Some("1.2.840.113549.3.7"), vec!["encrypt", "decrypt"]),
        "DES" => (Primitive::BlockCipher, "DES", Some("cbc"), Some("56"), None, Some(56), 0, Some("1.3.14.3.2.7"), vec!["encrypt", "decrypt"]),
        "Blowfish" => (Primitive::BlockCipher, "Blowfish", None, None, None, None, 0, None, vec!["encrypt", "decrypt"]),
        "RC4" => (Primitive::StreamCipher, "RC4", None, None, None, None, 0, None, vec!["encrypt", "decrypt"]),
        "Salsa20" => (Primitive::StreamCipher, "Salsa20", None, Some("256"), None, Some(256), 0, None, vec!["encrypt", "decrypt"]),

        // Hash functions
        "SHA-256" => (Primitive::Hash, "SHA-2", None, Some("256"), None, Some(128), 0, Some("2.16.840.1.101.3.4.2.1"), vec!["digest"]),
        "SHA-384" => (Primitive::Hash, "SHA-2", None, Some("384"), None, Some(192), 0, Some("2.16.840.1.101.3.4.2.2"), vec!["digest"]),
        "SHA-512" => (Primitive::Hash, "SHA-2", None, Some("512"), None, Some(256), 0, Some("2.16.840.1.101.3.4.2.3"), vec!["digest"]),
        "SHA-1" => (Primitive::Hash, "SHA-1", None, Some("160"), None, Some(80), 0, Some("1.3.14.3.2.26"), vec!["digest"]),
        "SHA-3" => (Primitive::Hash, "SHA-3", None, None, None, Some(128), 0, Some("2.16.840.1.101.3.4.2.8"), vec!["digest"]),
        "MD5" => (Primitive::Hash, "MD5", None, Some("128"), None, Some(64), 0, Some("1.2.840.113549.2.5"), vec!["digest"]),
        "BLAKE2" | "BLAKE2b" | "BLAKE2s" => (Primitive::Hash, "BLAKE2", None, None, None, Some(128), 0, None, vec!["digest"]),
        "BLAKE3" => (Primitive::Hash, "BLAKE3", None, None, None, Some(128), 0, None, vec!["digest"]),

        // Asymmetric / Signatures
        "RSA" => (Primitive::Pke, "RSA", None, Some("2048"), None, Some(112), 0, Some("1.2.840.113549.1.1.1"), vec!["keygen", "encrypt", "decrypt", "sign", "verify"]),
        "ECDSA" | "ECDSA-P256" => (Primitive::Signature, "ECDSA", None, Some("256"), Some("nist/P-256"), Some(128), 0, Some("1.2.840.10045.2.1"), vec!["keygen", "sign", "verify"]),
        "ECDSA-P384" => (Primitive::Signature, "ECDSA", None, Some("384"), Some("nist/P-384"), Some(192), 0, Some("1.2.840.10045.2.1"), vec!["keygen", "sign", "verify"]),
        "ECDSA-P521" => (Primitive::Signature, "ECDSA", None, Some("521"), Some("nist/P-521"), Some(256), 0, Some("1.2.840.10045.2.1"), vec!["keygen", "sign", "verify"]),
        "Ed25519" => (Primitive::Signature, "EdDSA", None, None, Some("edwards/Ed25519"), Some(128), 0, Some("1.3.101.112"), vec!["keygen", "sign", "verify"]),
        "Ed448" => (Primitive::Signature, "EdDSA", None, None, Some("edwards/Ed448"), Some(224), 0, Some("1.3.101.113"), vec!["keygen", "sign", "verify"]),
        "DSA" => (Primitive::Signature, "DSA", None, Some("2048"), None, Some(112), 0, Some("1.2.840.10040.4.1"), vec!["sign", "verify"]),

        // Key exchange
        "ECDH" => (Primitive::KeyAgree, "ECDH", None, None, Some("nist/P-256"), Some(128), 0, None, vec!["keygen", "keyderive"]),
        "X25519" => (Primitive::KeyAgree, "X25519", None, None, Some("montgomery/Curve25519"), Some(128), 0, Some("1.3.101.110"), vec!["keygen", "keyderive"]),
        "DH" => (Primitive::KeyAgree, "DH", None, Some("2048"), None, Some(112), 0, None, vec!["keygen", "keyderive"]),

        // MACs
        "HMAC" | "HMAC-SHA256" | "HMAC-SHA512" => (Primitive::Mac, "HMAC", None, None, None, Some(128), 0, Some("1.2.840.113549.2.9"), vec!["keygen", "sign", "verify"]),
        "HMAC-SHA1" => (Primitive::Mac, "HMAC", None, None, None, Some(80), 0, None, vec!["keygen", "sign", "verify"]),

        // KDFs
        "HKDF" => (Primitive::Kdf, "HKDF", None, None, None, None, 0, None, vec!["keyderive"]),
        "PBKDF2" => (Primitive::Kdf, "PBKDF2", None, None, None, None, 0, None, vec!["keyderive"]),
        "scrypt" => (Primitive::Kdf, "scrypt", None, None, None, None, 0, None, vec!["keyderive"]),
        "bcrypt" => (Primitive::Kdf, "bcrypt", None, None, None, None, 0, None, vec!["keyderive"]),
        "Argon2" => (Primitive::Kdf, "Argon2", None, None, None, None, 0, None, vec!["keyderive"]),

        // Post-quantum
        "ML-KEM" => (Primitive::Kem, "ML-KEM", None, None, None, Some(256), 5, Some("2.16.840.1.101.3.4.4"), vec!["keygen", "encapsulate", "decapsulate"]),
        "ML-DSA" => (Primitive::Signature, "ML-DSA", None, None, None, Some(256), 5, None, vec!["keygen", "sign", "verify"]),

        // Generic / library-level detections
        _ => (Primitive::Unknown, name, None, None, None, None, 0, None, vec![]),
    };

    CryptoAlgorithm {
        name: name.to_string(),
        algorithm_family: family.to_string(),
        primitive,
        parameter_set: param_set.map(|s| s.to_string()),
        elliptic_curve: curve.map(|s| s.to_string()),
        mode: mode.map(|s| s.to_string()),
        oid: oid.map(|s| s.to_string()),
        classical_security_level: security,
        nist_quantum_security_level: quantum,
        fips_status,
        crypto_functions: functions.into_iter().map(|s| s.to_string()).collect(),
    }
}

fn confidence_rank(c: &Confidence) -> u8 {
    match c {
        Confidence::High => 3,
        Confidence::Medium => 2,
        Confidence::Low => 1,
    }
}

fn deduplicate_findings(findings: Vec<CryptoFinding>) -> Vec<CryptoFinding> {
    let mut best: HashMap<String, CryptoFinding> = HashMap::new();

    for finding in findings {
        let key = format!(
            "{}:{}:{}",
            finding.algorithm.name, finding.file_path, finding.line_number
        );
        best.entry(key)
            .and_modify(|existing| {
                if confidence_rank(&finding.confidence) > confidence_rank(&existing.confidence) {
                    *existing = finding.clone();
                }
            })
            .or_insert(finding);
    }

    best.into_values().collect()
}

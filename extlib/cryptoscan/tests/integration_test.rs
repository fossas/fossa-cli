use assert_cmd::Command;
use serde_json::Value;
use std::path::PathBuf;

fn fixture_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../test-fixtures")
        .join(name)
}

fn run_scan(fixture: &str, ecosystem: &str) -> Vec<Value> {
    let output = Command::cargo_bin("cryptoscan")
        .unwrap()
        .args(["--path", fixture_path(fixture).to_str().unwrap()])
        .args(["--ecosystem", ecosystem])
        .args(["--format", "json"])
        .output()
        .expect("failed to execute");
    assert!(output.status.success(), "scanner exited with error");
    serde_json::from_slice(&output.stdout).expect("invalid JSON output")
}

fn run_scan_non_fips(fixture: &str, ecosystem: &str) -> Vec<Value> {
    let output = Command::cargo_bin("cryptoscan")
        .unwrap()
        .args(["--path", fixture_path(fixture).to_str().unwrap()])
        .args(["--ecosystem", ecosystem])
        .args(["--format", "json"])
        .arg("--non-fips-only")
        .output()
        .expect("failed to execute");
    assert!(output.status.success(), "scanner exited with error");
    serde_json::from_slice(&output.stdout).expect("invalid JSON output")
}

fn run_scan_cyclonedx(fixture: &str, ecosystem: &str) -> Value {
    let output = Command::cargo_bin("cryptoscan")
        .unwrap()
        .args(["--path", fixture_path(fixture).to_str().unwrap()])
        .args(["--ecosystem", ecosystem])
        .args(["--format", "cyclonedx"])
        .output()
        .expect("failed to execute");
    assert!(output.status.success(), "scanner exited with error");
    serde_json::from_value(serde_json::from_slice(&output.stdout).expect("invalid JSON"))
        .expect("invalid CycloneDX structure")
}

fn has_algorithm(findings: &[Value], name: &str) -> bool {
    findings.iter().any(|f| {
        f["algorithm"]["name"].as_str().unwrap_or("") == name
    })
}

fn has_algorithm_with_status(findings: &[Value], name: &str, status: &str) -> bool {
    findings.iter().any(|f| {
        f["algorithm"]["name"].as_str().unwrap_or("") == name
            && f["algorithm"]["fips_status"].as_str().unwrap_or("") == status
    })
}

fn has_algorithm_with_method(findings: &[Value], name: &str, method: &str) -> bool {
    findings.iter().any(|f| {
        f["algorithm"]["name"].as_str().unwrap_or("") == name
            && f["detection_method"].as_str().unwrap_or("") == method
    })
}

// ============================================================================
// Python ecosystem tests
// ============================================================================

#[test]
fn python_detects_aes_encryption() {
    let findings = run_scan("python-web-app", "python");
    assert!(has_algorithm(&findings, "AES"), "should detect AES");
    assert!(has_algorithm_with_status(&findings, "AES", "approved"));
}

#[test]
fn python_detects_aes_gcm_api_call() {
    let findings = run_scan("python-web-app", "python");
    assert!(has_algorithm_with_method(&findings, "AES-GCM", "api-call"),
        "should detect AES-GCM via API call pattern");
}

#[test]
fn python_detects_chacha20() {
    let findings = run_scan("python-web-app", "python");
    assert!(has_algorithm_with_status(&findings, "ChaCha20-Poly1305", "not-approved"),
        "ChaCha20-Poly1305 should be not-approved");
}

#[test]
fn python_detects_rsa() {
    let findings = run_scan("python-web-app", "python");
    assert!(has_algorithm_with_status(&findings, "RSA", "approved"));
}

#[test]
fn python_detects_ecdsa_p256() {
    let findings = run_scan("python-web-app", "python");
    assert!(has_algorithm(&findings, "ECDSA-P256"), "should detect ECDSA P-256");
}

#[test]
fn python_detects_ed25519() {
    let findings = run_scan("python-web-app", "python");
    assert!(has_algorithm_with_status(&findings, "Ed25519", "approved"));
}

#[test]
fn python_detects_hash_algorithms() {
    let findings = run_scan("python-web-app", "python");
    assert!(has_algorithm_with_status(&findings, "SHA-256", "approved"));
    assert!(has_algorithm_with_status(&findings, "SHA-1", "deprecated"));
    assert!(has_algorithm_with_status(&findings, "MD5", "not-approved"));
    assert!(has_algorithm_with_status(&findings, "BLAKE2", "not-approved"));
}

#[test]
fn python_detects_kdf_algorithms() {
    let findings = run_scan("python-web-app", "python");
    assert!(has_algorithm_with_status(&findings, "PBKDF2", "approved"));
    assert!(has_algorithm_with_status(&findings, "scrypt", "not-approved"));
    assert!(has_algorithm_with_status(&findings, "HKDF", "approved"));
}

#[test]
fn python_detects_bcrypt() {
    let findings = run_scan("python-web-app", "python");
    assert!(has_algorithm(&findings, "bcrypt"), "should detect bcrypt import");
}

#[test]
fn python_detects_dependency_manifests() {
    let findings = run_scan("python-web-app", "python");
    let dep_findings: Vec<&Value> = findings.iter()
        .filter(|f| f["detection_method"].as_str() == Some("dependency-manifest"))
        .collect();
    assert!(dep_findings.len() >= 3, "should detect at least 3 dependency entries");
}

#[test]
fn python_total_finding_count() {
    let findings = run_scan("python-web-app", "python");
    assert!(findings.len() >= 20, "Python fixture should produce at least 20 findings, got {}", findings.len());
}

// ============================================================================
// Java ecosystem tests
// ============================================================================

#[test]
fn java_detects_aes_gcm() {
    let findings = run_scan("java-microservice", "java");
    assert!(has_algorithm_with_status(&findings, "AES-256-GCM", "approved"),
        "should detect AES-256-GCM via Cipher.getInstance");
}

#[test]
fn java_detects_aes_ecb_deprecated() {
    let findings = run_scan("java-microservice", "java");
    assert!(has_algorithm_with_status(&findings, "AES-ECB", "deprecated"),
        "AES-ECB should be marked as deprecated");
}

#[test]
fn java_detects_3des_deprecated() {
    let findings = run_scan("java-microservice", "java");
    assert!(has_algorithm_with_status(&findings, "3DES", "deprecated"),
        "3DES should be marked as deprecated");
}

#[test]
fn java_detects_signatures() {
    let findings = run_scan("java-microservice", "java");
    assert!(has_algorithm_with_status(&findings, "RSA", "approved"));
    assert!(has_algorithm_with_status(&findings, "ECDSA", "approved"));
    assert!(has_algorithm_with_status(&findings, "Ed25519", "approved"));
}

#[test]
fn java_detects_key_exchange() {
    let findings = run_scan("java-microservice", "java");
    assert!(has_algorithm_with_status(&findings, "DH", "approved"), "should detect DH key exchange");
    assert!(has_algorithm_with_status(&findings, "X25519", "not-approved"), "X25519 should be not-approved");
}

#[test]
fn java_detects_hashes() {
    let findings = run_scan("java-microservice", "java");
    assert!(has_algorithm_with_status(&findings, "SHA-256", "approved"));
    assert!(has_algorithm_with_status(&findings, "SHA-1", "deprecated"));
    assert!(has_algorithm_with_status(&findings, "MD5", "not-approved"));
}

#[test]
fn java_detects_hmac_and_pbkdf2() {
    let findings = run_scan("java-microservice", "java");
    assert!(has_algorithm_with_status(&findings, "HMAC-SHA256", "approved"));
    assert!(has_algorithm_with_status(&findings, "PBKDF2", "approved"));
}

#[test]
fn java_detects_bouncycastle_dependency() {
    let findings = run_scan("java-microservice", "java");
    assert!(has_algorithm_with_method(&findings, "BouncyCastle", "dependency-manifest"),
        "should detect BouncyCastle in pom.xml");
}

#[test]
fn java_total_finding_count() {
    let findings = run_scan("java-microservice", "java");
    assert!(findings.len() >= 15, "Java fixture should produce at least 15 findings, got {}", findings.len());
}

// ============================================================================
// Go ecosystem tests
// ============================================================================

#[test]
fn go_detects_aes_gcm() {
    let findings = run_scan("go-api-server", "go");
    assert!(has_algorithm(&findings, "AES"), "should detect AES imports");
    assert!(has_algorithm_with_method(&findings, "AES-GCM", "api-call"),
        "should detect cipher.NewGCM API call");
}

#[test]
fn go_detects_chacha20() {
    let findings = run_scan("go-api-server", "go");
    assert!(has_algorithm_with_status(&findings, "ChaCha20-Poly1305", "not-approved"));
}

#[test]
fn go_detects_signatures() {
    let findings = run_scan("go-api-server", "go");
    assert!(has_algorithm(&findings, "RSA"));
    assert!(has_algorithm(&findings, "ECDSA"));
    assert!(has_algorithm(&findings, "Ed25519"));
}

#[test]
fn go_detects_ecdsa_curves() {
    let findings = run_scan("go-api-server", "go");
    assert!(has_algorithm(&findings, "ECDSA-P256"), "should detect P-256 curve");
    assert!(has_algorithm(&findings, "ECDSA-P384"), "should detect P-384 curve");
}

#[test]
fn go_detects_x25519() {
    let findings = run_scan("go-api-server", "go");
    assert!(has_algorithm_with_status(&findings, "X25519", "not-approved"));
}

#[test]
fn go_detects_hash_algorithms() {
    let findings = run_scan("go-api-server", "go");
    assert!(has_algorithm_with_status(&findings, "SHA-256", "approved"));
    assert!(has_algorithm_with_status(&findings, "SHA-512", "approved"));
    assert!(has_algorithm_with_status(&findings, "SHA-1", "deprecated"));
    assert!(has_algorithm_with_status(&findings, "MD5", "not-approved"));
    assert!(has_algorithm_with_status(&findings, "BLAKE2b", "not-approved"));
}

#[test]
fn go_detects_password_hashing() {
    let findings = run_scan("go-api-server", "go");
    assert!(has_algorithm_with_status(&findings, "Argon2", "not-approved"));
    assert!(has_algorithm_with_status(&findings, "bcrypt", "not-approved"));
    assert!(has_algorithm_with_status(&findings, "scrypt", "not-approved"));
}

#[test]
fn go_detects_kdf() {
    let findings = run_scan("go-api-server", "go");
    assert!(has_algorithm_with_status(&findings, "HKDF", "approved"));
    assert!(has_algorithm_with_status(&findings, "HMAC", "approved"));
}

#[test]
fn go_total_finding_count() {
    let findings = run_scan("go-api-server", "go");
    assert!(findings.len() >= 30, "Go fixture should produce at least 30 findings, got {}", findings.len());
}

// ============================================================================
// Node.js ecosystem tests
// ============================================================================

#[test]
fn node_detects_aes_variants() {
    let findings = run_scan("node-auth-service", "node");
    assert!(has_algorithm_with_status(&findings, "AES-256-GCM", "approved"));
    assert!(has_algorithm_with_status(&findings, "AES-128-CBC", "approved"));
}

#[test]
fn node_detects_3des() {
    let findings = run_scan("node-auth-service", "node");
    assert!(has_algorithm_with_status(&findings, "3DES", "deprecated"),
        "3DES via des-ede3-cbc should be deprecated");
}

#[test]
fn node_detects_chacha20() {
    let findings = run_scan("node-auth-service", "node");
    assert!(has_algorithm_with_status(&findings, "ChaCha20-Poly1305", "not-approved"));
}

#[test]
fn node_detects_hashes() {
    let findings = run_scan("node-auth-service", "node");
    assert!(has_algorithm_with_status(&findings, "SHA-256", "approved"));
    assert!(has_algorithm_with_status(&findings, "SHA-1", "deprecated"));
    assert!(has_algorithm_with_status(&findings, "MD5", "not-approved"));
}

#[test]
fn node_detects_hmac_variants() {
    let findings = run_scan("node-auth-service", "node");
    assert!(has_algorithm_with_status(&findings, "HMAC-SHA256", "approved"));
    assert!(has_algorithm_with_status(&findings, "HMAC-SHA1", "deprecated"));
}

#[test]
fn node_detects_key_generation() {
    let findings = run_scan("node-auth-service", "node");
    assert!(has_algorithm_with_status(&findings, "RSA", "approved"));
    assert!(has_algorithm_with_status(&findings, "Ed25519", "approved"));
    assert!(has_algorithm_with_status(&findings, "X25519", "not-approved"));
}

#[test]
fn node_detects_kdf() {
    let findings = run_scan("node-auth-service", "node");
    assert!(has_algorithm_with_status(&findings, "PBKDF2", "approved"));
    assert!(has_algorithm_with_status(&findings, "scrypt", "not-approved"));
}

#[test]
fn node_detects_package_json_deps() {
    let findings = run_scan("node-auth-service", "node");
    assert!(has_algorithm_with_method(&findings, "bcrypt", "dependency-manifest"));
    assert!(has_algorithm_with_method(&findings, "Argon2", "dependency-manifest"));
}

#[test]
fn node_total_finding_count() {
    let findings = run_scan("node-auth-service", "node");
    assert!(findings.len() >= 15, "Node fixture should produce at least 15 findings, got {}", findings.len());
}

// ============================================================================
// Rust ecosystem tests
// ============================================================================

#[test]
fn rust_detects_aes_gcm() {
    let findings = run_scan("rust-crypto-tool", "rust");
    assert!(has_algorithm_with_status(&findings, "AES-GCM", "approved"));
}

#[test]
fn rust_detects_chacha20() {
    let findings = run_scan("rust-crypto-tool", "rust");
    assert!(has_algorithm_with_status(&findings, "ChaCha20-Poly1305", "not-approved"));
}

#[test]
fn rust_detects_sha256() {
    let findings = run_scan("rust-crypto-tool", "rust");
    assert!(has_algorithm_with_status(&findings, "SHA-256", "approved"));
}

#[test]
fn rust_detects_blake_variants() {
    let findings = run_scan("rust-crypto-tool", "rust");
    assert!(has_algorithm_with_status(&findings, "BLAKE2", "not-approved"));
    assert!(has_algorithm_with_status(&findings, "BLAKE3", "not-approved"));
}

#[test]
fn rust_detects_signatures_and_kex() {
    let findings = run_scan("rust-crypto-tool", "rust");
    assert!(has_algorithm_with_status(&findings, "Ed25519", "approved"));
    assert!(has_algorithm_with_status(&findings, "X25519", "not-approved"));
}

#[test]
fn rust_detects_argon2() {
    let findings = run_scan("rust-crypto-tool", "rust");
    assert!(has_algorithm_with_status(&findings, "Argon2", "not-approved"));
}

#[test]
fn rust_detects_hkdf() {
    let findings = run_scan("rust-crypto-tool", "rust");
    assert!(has_algorithm_with_status(&findings, "HKDF", "approved"));
}

#[test]
fn rust_detects_cargo_deps() {
    let findings = run_scan("rust-crypto-tool", "rust");
    let dep_findings: Vec<&Value> = findings.iter()
        .filter(|f| f["detection_method"].as_str() == Some("dependency-manifest"))
        .collect();
    assert!(dep_findings.len() >= 5, "should detect at least 5 Cargo.toml deps, got {}", dep_findings.len());
}

#[test]
fn rust_detects_ring_dependency() {
    let findings = run_scan("rust-crypto-tool", "rust");
    assert!(has_algorithm_with_method(&findings, "ring", "dependency-manifest"),
        "should detect ring crate in Cargo.toml");
}

#[test]
fn rust_total_finding_count() {
    let findings = run_scan("rust-crypto-tool", "rust");
    assert!(findings.len() >= 15, "Rust fixture should produce at least 15 findings, got {}", findings.len());
}

// ============================================================================
// Cross-ecosystem / feature tests
// ============================================================================

#[test]
fn non_fips_only_filters_approved() {
    let all = run_scan("go-api-server", "go");
    let non_fips = run_scan_non_fips("go-api-server", "go");

    assert!(non_fips.len() < all.len(),
        "non-FIPS-only should return fewer findings than full scan");

    for finding in &non_fips {
        let status = finding["algorithm"]["fips_status"].as_str().unwrap();
        assert_ne!(status, "approved",
            "non-FIPS-only should not include approved algorithms, found: {}",
            finding["algorithm"]["name"]);
    }
}

#[test]
fn cyclonedx_output_structure() {
    let bom = run_scan_cyclonedx("node-auth-service", "node");

    assert_eq!(bom["bomFormat"], "CycloneDX");
    assert_eq!(bom["specVersion"], "1.7");
    assert!(bom["serialNumber"].as_str().unwrap().starts_with("urn:uuid:"));
    assert_eq!(bom["version"], 1);
    assert!(bom["metadata"]["timestamp"].as_str().is_some());
    assert_eq!(bom["metadata"]["tools"][0]["name"], "fossa-cryptoscan");
}

#[test]
fn cyclonedx_contains_crypto_components() {
    let bom = run_scan_cyclonedx("node-auth-service", "node");
    let components = bom["components"].as_array().expect("components should be array");

    let crypto_components: Vec<&Value> = components.iter()
        .filter(|c| c["type"] == "cryptographic-asset")
        .collect();
    assert!(!crypto_components.is_empty(), "should contain cryptographic-asset components");

    // Verify crypto properties structure
    let first_crypto = crypto_components[0];
    assert!(first_crypto["cryptoProperties"]["assetType"].as_str().is_some());
    assert!(first_crypto["cryptoProperties"]["algorithmProperties"]["primitive"].as_str().is_some());
    assert!(first_crypto["cryptoProperties"]["algorithmProperties"]["algorithmFamily"].as_str().is_some());
}

#[test]
fn cyclonedx_contains_fossa_properties() {
    let bom = run_scan_cyclonedx("node-auth-service", "node");
    let components = bom["components"].as_array().unwrap();

    let crypto_component = components.iter()
        .find(|c| c["type"] == "cryptographic-asset")
        .expect("should have a crypto component");

    let properties = crypto_component["properties"].as_array().unwrap();
    let prop_names: Vec<&str> = properties.iter()
        .map(|p| p["name"].as_str().unwrap())
        .collect();

    assert!(prop_names.contains(&"fossa:fips-status"), "should include fossa:fips-status");
    assert!(prop_names.contains(&"fossa:detected-in"), "should include fossa:detected-in");
    assert!(prop_names.contains(&"fossa:detection-method"), "should include fossa:detection-method");
    assert!(prop_names.contains(&"fossa:ecosystem"), "should include fossa:ecosystem");
}

#[test]
fn cyclonedx_contains_dependencies() {
    let bom = run_scan_cyclonedx("node-auth-service", "node");
    let deps = bom["dependencies"].as_array();
    assert!(deps.is_some(), "CycloneDX output should contain dependencies section");
}

#[test]
fn auto_ecosystem_detection() {
    // Test that --ecosystem auto works (should detect from manifest files)
    let output = Command::cargo_bin("cryptoscan")
        .unwrap()
        .args(["--path", fixture_path("python-web-app").to_str().unwrap()])
        .args(["--ecosystem", "auto"])
        .args(["--format", "json"])
        .output()
        .expect("failed to execute");
    assert!(output.status.success());
    let findings: Vec<Value> = serde_json::from_slice(&output.stdout).unwrap();
    assert!(!findings.is_empty(), "auto-detection should find the Python ecosystem");
}

#[test]
fn finding_includes_file_location() {
    let findings = run_scan("python-web-app", "python");
    let api_finding = findings.iter()
        .find(|f| f["detection_method"] == "api-call")
        .expect("should have at least one api-call finding");

    assert!(api_finding["file_path"].as_str().is_some(), "should have file_path");
    assert!(api_finding["line_number"].as_u64().unwrap() > 0, "should have positive line_number");
    assert!(api_finding["matched_text"].as_str().is_some(), "should have matched_text");
}

#[test]
fn finding_includes_algorithm_details() {
    let findings = run_scan("go-api-server", "go");
    let aes_finding = findings.iter()
        .find(|f| f["algorithm"]["name"] == "AES-GCM")
        .expect("should find AES-GCM");

    let algo = &aes_finding["algorithm"];
    assert_eq!(algo["algorithm_family"], "AES");
    assert_eq!(algo["primitive"], "ae");
    assert_eq!(algo["fips_status"], "approved");
    assert!(algo["oid"].as_str().is_some(), "should include OID");
    assert!(algo["classical_security_level"].as_u64().is_some(), "should include security level");

    let functions = algo["crypto_functions"].as_array().unwrap();
    assert!(!functions.is_empty(), "should list crypto functions");
}

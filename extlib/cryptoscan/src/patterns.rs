use regex::Regex;

use crate::crypto_algorithm::{Confidence, DetectionMethod};

/// A pattern rule that matches crypto usage in source code.
#[derive(Debug, Clone)]
pub struct CryptoPattern {
    pub regex: Regex,
    pub algorithm_name: String,
    pub detection_method: DetectionMethod,
    pub ecosystem: &'static str,
    pub file_extensions: Vec<&'static str>,
    pub providing_library: Option<String>,
    pub confidence: Confidence,
}

/// Build the full pattern database for all ecosystems.
pub fn build_patterns() -> Vec<CryptoPattern> {
    let mut patterns = Vec::new();
    patterns.extend(python_patterns());
    patterns.extend(java_patterns());
    patterns.extend(go_patterns());
    patterns.extend(node_patterns());
    patterns.extend(rust_patterns());
    patterns.extend(ruby_patterns());
    patterns.extend(csharp_patterns());
    patterns.extend(php_patterns());
    patterns.extend(swift_patterns());
    patterns.extend(elixir_patterns());
    patterns.extend(cross_ecosystem_patterns());
    patterns
}

/// Return manifest file names relevant to a given ecosystem.
pub fn ecosystem_manifests(ecosystem: &str) -> Vec<&'static str> {
    match ecosystem {
        "python" => vec![
            "requirements.txt",
            "Pipfile",
            "pyproject.toml",
            "setup.py",
            "setup.cfg",
            "conda.yaml",
            "environment.yml",
        ],
        "java" => vec!["pom.xml", "build.gradle", "build.gradle.kts"],
        "go" => vec!["go.mod", "go.sum"],
        "node" | "javascript" | "typescript" => vec!["package.json", "package-lock.json", "yarn.lock"],
        "rust" => vec!["Cargo.toml", "Cargo.lock"],
        "ruby" => vec!["Gemfile", "Gemfile.lock", "*.gemspec"],
        "csharp" | "dotnet" => vec!["*.csproj", "*.fsproj", "packages.config", "Directory.Packages.props"],
        "php" => vec!["composer.json", "composer.lock"],
        "swift" => vec!["Package.swift", "Podfile", "Podfile.lock"],
        "elixir" => vec!["mix.exs", "mix.lock"],
        _ => vec![],
    }
}

// ─── Python Patterns ──────────────────────────────────────────────────

fn python_patterns() -> Vec<CryptoPattern> {
    vec![
        // -- Imports --
        pat(r"from\s+cryptography\.hazmat\.primitives\.ciphers\s+import", "AES", DetectionMethod::ImportStatement, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"from\s+cryptography\.hazmat\.primitives\.ciphers\.algorithms\s+import\s+(\w+)", "AES", DetectionMethod::ImportStatement, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"from\s+cryptography\.hazmat\.primitives\.hashes\s+import", "SHA-256", DetectionMethod::ImportStatement, "python", &["py"], Some("cryptography"), Confidence::Medium),
        pat(r"from\s+cryptography\.hazmat\.primitives\.asymmetric\s+import\s+(rsa|ec|ed25519|ed448|dsa|dh|x25519|x448|padding)", "RSA", DetectionMethod::ImportStatement, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"from\s+cryptography\.hazmat\.primitives\.kdf\s+import\s+(hkdf|pbkdf2|scrypt|concatkdf|x963kdf)", "HKDF", DetectionMethod::ImportStatement, "python", &["py"], Some("cryptography"), Confidence::High),

        // -- API calls --
        pat(r"algorithms\.AES\b", "AES", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"algorithms\.AES128\b", "AES-128", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"algorithms\.AES256\b", "AES-256", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"algorithms\.ChaCha20\b", "ChaCha20-Poly1305", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"algorithms\.TripleDES\b", "3DES", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"algorithms\.Blowfish\b", "Blowfish", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"algorithms\.ARC4\b", "RC4", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"modes\.GCM\b", "AES-GCM", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"modes\.CBC\b", "AES-CBC", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"modes\.CTR\b", "AES-CTR", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"modes\.ECB\b", "AES-ECB", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"hashes\.SHA256\b", "SHA-256", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"hashes\.SHA384\b", "SHA-384", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"hashes\.SHA512\b", "SHA-512", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"hashes\.SHA1\b", "SHA-1", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"hashes\.MD5\b", "MD5", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"hashes\.BLAKE2[bs]\b", "BLAKE2", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"hashes\.SHA3_\d+\b", "SHA-3", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"ec\.ECDSA\b", "ECDSA", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"ec\.ECDH\b", "ECDH", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"ec\.SECP256R1\b", "ECDSA-P256", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"ec\.SECP384R1\b", "ECDSA-P384", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"ed25519\.Ed25519", "Ed25519", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"ed448\.Ed448", "Ed448", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"x25519\.X25519", "X25519", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"rsa\.generate_private_key\b", "RSA", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"dh\.generate_parameters\b", "DH", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"PBKDF2HMAC\b", "PBKDF2", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"Scrypt\b", "scrypt", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),
        pat(r"HKDF\b", "HKDF", DetectionMethod::ApiCall, "python", &["py"], Some("cryptography"), Confidence::High),

        // -- hashlib --
        pat(r"hashlib\.sha256\b", "SHA-256", DetectionMethod::ApiCall, "python", &["py"], Some("hashlib"), Confidence::High),
        pat(r"hashlib\.sha384\b", "SHA-384", DetectionMethod::ApiCall, "python", &["py"], Some("hashlib"), Confidence::High),
        pat(r"hashlib\.sha512\b", "SHA-512", DetectionMethod::ApiCall, "python", &["py"], Some("hashlib"), Confidence::High),
        pat(r"hashlib\.sha1\b", "SHA-1", DetectionMethod::ApiCall, "python", &["py"], Some("hashlib"), Confidence::High),
        pat(r"hashlib\.md5\b", "MD5", DetectionMethod::ApiCall, "python", &["py"], Some("hashlib"), Confidence::High),
        pat(r"hashlib\.blake2[bs]\b", "BLAKE2", DetectionMethod::ApiCall, "python", &["py"], Some("hashlib"), Confidence::High),
        pat(r#"hashlib\.new\s*\(\s*["'](\w+)["']"#, "SHA-256", DetectionMethod::ApiCall, "python", &["py"], Some("hashlib"), Confidence::Medium),

        // -- PyCryptodome --
        pat(r"from\s+Crypto\.Cipher\s+import\s+AES\b", "AES", DetectionMethod::ImportStatement, "python", &["py"], Some("pycryptodome"), Confidence::High),
        pat(r"from\s+Crypto\.Cipher\s+import\s+DES\b", "DES", DetectionMethod::ImportStatement, "python", &["py"], Some("pycryptodome"), Confidence::High),
        pat(r"from\s+Crypto\.Cipher\s+import\s+DES3\b", "3DES", DetectionMethod::ImportStatement, "python", &["py"], Some("pycryptodome"), Confidence::High),
        pat(r"from\s+Crypto\.Cipher\s+import\s+Blowfish\b", "Blowfish", DetectionMethod::ImportStatement, "python", &["py"], Some("pycryptodome"), Confidence::High),
        pat(r"from\s+Crypto\.Cipher\s+import\s+ChaCha20\b", "ChaCha20", DetectionMethod::ImportStatement, "python", &["py"], Some("pycryptodome"), Confidence::High),
        pat(r"from\s+Crypto\.Cipher\s+import\s+ARC4\b", "RC4", DetectionMethod::ImportStatement, "python", &["py"], Some("pycryptodome"), Confidence::High),
        pat(r"from\s+Crypto\.Hash\s+import\s+SHA256\b", "SHA-256", DetectionMethod::ImportStatement, "python", &["py"], Some("pycryptodome"), Confidence::High),
        pat(r"from\s+Crypto\.Hash\s+import\s+MD5\b", "MD5", DetectionMethod::ImportStatement, "python", &["py"], Some("pycryptodome"), Confidence::High),

        // -- bcrypt/argon2 --
        pat(r"import\s+bcrypt\b", "bcrypt", DetectionMethod::ImportStatement, "python", &["py"], Some("bcrypt"), Confidence::High),
        pat(r"import\s+argon2\b", "Argon2", DetectionMethod::ImportStatement, "python", &["py"], Some("argon2-cffi"), Confidence::High),
        pat(r"argon2\.PasswordHasher\b", "Argon2", DetectionMethod::ApiCall, "python", &["py"], Some("argon2-cffi"), Confidence::High),

        // -- Dependency manifests --
        pat(r"(?m)^cryptography[>=<!\s]", "cryptography-lib", DetectionMethod::DependencyManifest, "python", &["txt", "cfg", "toml"], None, Confidence::Medium),
        pat(r"(?m)^pycryptodome[>=<!\s]", "pycryptodome-lib", DetectionMethod::DependencyManifest, "python", &["txt", "cfg", "toml"], None, Confidence::Medium),
        pat(r"(?m)^bcrypt[>=<!\s]", "bcrypt-lib", DetectionMethod::DependencyManifest, "python", &["txt", "cfg", "toml"], None, Confidence::Medium),
        pat(r"(?m)^argon2-cffi[>=<!\s]", "argon2-lib", DetectionMethod::DependencyManifest, "python", &["txt", "cfg", "toml"], None, Confidence::Medium),
        pat(r"(?m)^PyNaCl[>=<!\s]", "nacl-lib", DetectionMethod::DependencyManifest, "python", &["txt", "cfg", "toml"], None, Confidence::Medium),
    ]
}

// ─── Java Patterns ────────────────────────────────────────────────────

fn java_patterns() -> Vec<CryptoPattern> {
    vec![
        // -- JCA/JCE API calls --
        pat(r#"Cipher\.getInstance\s*\(\s*"AES/GCM/NoPadding""#, "AES-256-GCM", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),
        pat(r#"Cipher\.getInstance\s*\(\s*"AES/CBC/PKCS5Padding""#, "AES-CBC", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),
        pat(r#"Cipher\.getInstance\s*\(\s*"AES/ECB"#, "AES-ECB", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),
        pat(r#"Cipher\.getInstance\s*\(\s*"AES"#, "AES", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),
        pat(r#"Cipher\.getInstance\s*\(\s*"DES"#, "DES", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),
        pat(r#"Cipher\.getInstance\s*\(\s*"DESede"#, "3DES", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),
        pat(r#"Cipher\.getInstance\s*\(\s*"Blowfish"#, "Blowfish", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),
        pat(r#"Cipher\.getInstance\s*\(\s*"RC4"#, "RC4", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),
        pat(r#"Cipher\.getInstance\s*\(\s*"ChaCha20"#, "ChaCha20-Poly1305", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),
        pat(r#"Cipher\.getInstance\s*\(\s*"RSA"#, "RSA", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),

        pat(r#"MessageDigest\.getInstance\s*\(\s*"SHA-256""#, "SHA-256", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("java.security"), Confidence::High),
        pat(r#"MessageDigest\.getInstance\s*\(\s*"SHA-384""#, "SHA-384", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("java.security"), Confidence::High),
        pat(r#"MessageDigest\.getInstance\s*\(\s*"SHA-512""#, "SHA-512", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("java.security"), Confidence::High),
        pat(r#"MessageDigest\.getInstance\s*\(\s*"SHA-1""#, "SHA-1", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("java.security"), Confidence::High),
        pat(r#"MessageDigest\.getInstance\s*\(\s*"MD5""#, "MD5", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("java.security"), Confidence::High),

        pat(r#"KeyPairGenerator\.getInstance\s*\(\s*"RSA""#, "RSA", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("java.security"), Confidence::High),
        pat(r#"KeyPairGenerator\.getInstance\s*\(\s*"EC""#, "ECDSA", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("java.security"), Confidence::High),
        pat(r#"KeyPairGenerator\.getInstance\s*\(\s*"DSA""#, "DSA", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("java.security"), Confidence::High),
        pat(r#"KeyPairGenerator\.getInstance\s*\(\s*"Ed25519""#, "Ed25519", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("java.security"), Confidence::High),
        pat(r#"KeyAgreement\.getInstance\s*\(\s*"ECDH""#, "ECDH", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),
        pat(r#"KeyAgreement\.getInstance\s*\(\s*"DH""#, "DH", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),
        pat(r#"KeyAgreement\.getInstance\s*\(\s*"X25519""#, "X25519", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),

        pat(r#"Mac\.getInstance\s*\(\s*"HmacSHA256""#, "HMAC-SHA256", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),
        pat(r#"Mac\.getInstance\s*\(\s*"HmacSHA512""#, "HMAC-SHA512", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),
        pat(r#"Mac\.getInstance\s*\(\s*"HmacSHA1""#, "HMAC-SHA1", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),

        pat(r#"SecretKeyFactory\.getInstance\s*\(\s*"PBKDF2"#, "PBKDF2", DetectionMethod::ApiCall, "java", &["java", "kt"], Some("javax.crypto"), Confidence::High),

        // -- Imports --
        pat(r"import\s+javax\.crypto\.", "JCA-crypto", DetectionMethod::ImportStatement, "java", &["java", "kt"], Some("javax.crypto"), Confidence::Medium),
        pat(r"import\s+java\.security\.", "JCA-security", DetectionMethod::ImportStatement, "java", &["java", "kt"], Some("java.security"), Confidence::Medium),
        pat(r"import\s+org\.bouncycastle\.", "BouncyCastle", DetectionMethod::ImportStatement, "java", &["java", "kt"], Some("BouncyCastle"), Confidence::High),

        // -- Dependency manifests --
        pat(r"org\.bouncycastle", "BouncyCastle", DetectionMethod::DependencyManifest, "java", &["xml", "gradle", "kts"], None, Confidence::High),
        pat(r"com\.google\.crypto\.tink", "Tink", DetectionMethod::DependencyManifest, "java", &["xml", "gradle", "kts"], None, Confidence::High),
    ]
}

// ─── Go Patterns ──────────────────────────────────────────────────────

fn go_patterns() -> Vec<CryptoPattern> {
    vec![
        // -- Standard library imports --
        pat(r#""crypto/aes""#, "AES", DetectionMethod::ImportStatement, "go", &["go"], Some("crypto/aes"), Confidence::High),
        pat(r#""crypto/des""#, "DES", DetectionMethod::ImportStatement, "go", &["go"], Some("crypto/des"), Confidence::High),
        pat(r#""crypto/cipher""#, "AES", DetectionMethod::ImportStatement, "go", &["go"], Some("crypto/cipher"), Confidence::Medium),
        pat(r#""crypto/sha256""#, "SHA-256", DetectionMethod::ImportStatement, "go", &["go"], Some("crypto/sha256"), Confidence::High),
        pat(r#""crypto/sha512""#, "SHA-512", DetectionMethod::ImportStatement, "go", &["go"], Some("crypto/sha512"), Confidence::High),
        pat(r#""crypto/sha1""#, "SHA-1", DetectionMethod::ImportStatement, "go", &["go"], Some("crypto/sha1"), Confidence::High),
        pat(r#""crypto/md5""#, "MD5", DetectionMethod::ImportStatement, "go", &["go"], Some("crypto/md5"), Confidence::High),
        pat(r#""crypto/rsa""#, "RSA", DetectionMethod::ImportStatement, "go", &["go"], Some("crypto/rsa"), Confidence::High),
        pat(r#""crypto/ecdsa""#, "ECDSA", DetectionMethod::ImportStatement, "go", &["go"], Some("crypto/ecdsa"), Confidence::High),
        pat(r#""crypto/ed25519""#, "Ed25519", DetectionMethod::ImportStatement, "go", &["go"], Some("crypto/ed25519"), Confidence::High),
        pat(r#""crypto/ecdh""#, "ECDH", DetectionMethod::ImportStatement, "go", &["go"], Some("crypto/ecdh"), Confidence::High),
        pat(r#""crypto/hmac""#, "HMAC", DetectionMethod::ImportStatement, "go", &["go"], Some("crypto/hmac"), Confidence::High),
        pat(r#""crypto/rc4""#, "RC4", DetectionMethod::ImportStatement, "go", &["go"], Some("crypto/rc4"), Confidence::High),

        // -- x/crypto --
        pat(r#""golang\.org/x/crypto/chacha20poly1305""#, "ChaCha20-Poly1305", DetectionMethod::ImportStatement, "go", &["go"], Some("x/crypto"), Confidence::High),
        pat(r#""golang\.org/x/crypto/blake2b""#, "BLAKE2b", DetectionMethod::ImportStatement, "go", &["go"], Some("x/crypto"), Confidence::High),
        pat(r#""golang\.org/x/crypto/blake2s""#, "BLAKE2s", DetectionMethod::ImportStatement, "go", &["go"], Some("x/crypto"), Confidence::High),
        pat(r#""golang\.org/x/crypto/argon2""#, "Argon2", DetectionMethod::ImportStatement, "go", &["go"], Some("x/crypto"), Confidence::High),
        pat(r#""golang\.org/x/crypto/bcrypt""#, "bcrypt", DetectionMethod::ImportStatement, "go", &["go"], Some("x/crypto"), Confidence::High),
        pat(r#""golang\.org/x/crypto/scrypt""#, "scrypt", DetectionMethod::ImportStatement, "go", &["go"], Some("x/crypto"), Confidence::High),
        pat(r#""golang\.org/x/crypto/curve25519""#, "X25519", DetectionMethod::ImportStatement, "go", &["go"], Some("x/crypto"), Confidence::High),
        pat(r#""golang\.org/x/crypto/nacl""#, "NaCl", DetectionMethod::ImportStatement, "go", &["go"], Some("x/crypto"), Confidence::High),
        pat(r#""golang\.org/x/crypto/salsa20""#, "Salsa20", DetectionMethod::ImportStatement, "go", &["go"], Some("x/crypto"), Confidence::High),
        pat(r#""golang\.org/x/crypto/sha3""#, "SHA-3", DetectionMethod::ImportStatement, "go", &["go"], Some("x/crypto"), Confidence::High),
        pat(r#""golang\.org/x/crypto/hkdf""#, "HKDF", DetectionMethod::ImportStatement, "go", &["go"], Some("x/crypto"), Confidence::High),
        pat(r#""golang\.org/x/crypto/pbkdf2""#, "PBKDF2", DetectionMethod::ImportStatement, "go", &["go"], Some("x/crypto"), Confidence::High),

        // -- API calls --
        pat(r"aes\.NewCipher\b", "AES", DetectionMethod::ApiCall, "go", &["go"], Some("crypto/aes"), Confidence::High),
        pat(r"cipher\.NewGCM\b", "AES-GCM", DetectionMethod::ApiCall, "go", &["go"], Some("crypto/cipher"), Confidence::High),
        pat(r"cipher\.NewCBCEncrypter\b", "AES-CBC", DetectionMethod::ApiCall, "go", &["go"], Some("crypto/cipher"), Confidence::High),
        pat(r"cipher\.NewCTR\b", "AES-CTR", DetectionMethod::ApiCall, "go", &["go"], Some("crypto/cipher"), Confidence::High),
        pat(r"rsa\.GenerateKey\b", "RSA", DetectionMethod::ApiCall, "go", &["go"], Some("crypto/rsa"), Confidence::High),
        pat(r"ecdsa\.GenerateKey\b", "ECDSA", DetectionMethod::ApiCall, "go", &["go"], Some("crypto/ecdsa"), Confidence::High),
        pat(r"ed25519\.GenerateKey\b", "Ed25519", DetectionMethod::ApiCall, "go", &["go"], Some("crypto/ed25519"), Confidence::High),
        pat(r"elliptic\.P256\b", "ECDSA-P256", DetectionMethod::ApiCall, "go", &["go"], Some("crypto/elliptic"), Confidence::High),
        pat(r"elliptic\.P384\b", "ECDSA-P384", DetectionMethod::ApiCall, "go", &["go"], Some("crypto/elliptic"), Confidence::High),
        pat(r"elliptic\.P521\b", "ECDSA-P521", DetectionMethod::ApiCall, "go", &["go"], Some("crypto/elliptic"), Confidence::High),
        pat(r"hmac\.New\b", "HMAC", DetectionMethod::ApiCall, "go", &["go"], Some("crypto/hmac"), Confidence::High),
        pat(r"sha256\.New\b", "SHA-256", DetectionMethod::ApiCall, "go", &["go"], Some("crypto/sha256"), Confidence::High),
        pat(r"sha512\.New\b", "SHA-512", DetectionMethod::ApiCall, "go", &["go"], Some("crypto/sha512"), Confidence::High),
        pat(r"sha1\.New\b", "SHA-1", DetectionMethod::ApiCall, "go", &["go"], Some("crypto/sha1"), Confidence::High),
        pat(r"md5\.New\b", "MD5", DetectionMethod::ApiCall, "go", &["go"], Some("crypto/md5"), Confidence::High),
        pat(r"chacha20poly1305\.New\b", "ChaCha20-Poly1305", DetectionMethod::ApiCall, "go", &["go"], Some("x/crypto"), Confidence::High),
        pat(r"argon2\.IDKey\b", "Argon2", DetectionMethod::ApiCall, "go", &["go"], Some("x/crypto"), Confidence::High),
        pat(r"bcrypt\.GenerateFromPassword\b", "bcrypt", DetectionMethod::ApiCall, "go", &["go"], Some("x/crypto"), Confidence::High),
        pat(r"scrypt\.Key\b", "scrypt", DetectionMethod::ApiCall, "go", &["go"], Some("x/crypto"), Confidence::High),

        // -- go.mod dependencies --
        pat(r"golang\.org/x/crypto", "x/crypto", DetectionMethod::DependencyManifest, "go", &["mod", "sum"], None, Confidence::Medium),
    ]
}

// ─── Node.js Patterns ─────────────────────────────────────────────────

fn node_patterns() -> Vec<CryptoPattern> {
    vec![
        // -- Built-in crypto --
        pat(r#"crypto\.createCipheriv\s*\(\s*['"]aes-256-gcm['"]"#, "AES-256-GCM", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r#"crypto\.createCipheriv\s*\(\s*['"]aes-256-cbc['"]"#, "AES-256-CBC", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r#"crypto\.createCipheriv\s*\(\s*['"]aes-128-gcm['"]"#, "AES-128-GCM", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r#"crypto\.createCipheriv\s*\(\s*['"]aes-128-cbc['"]"#, "AES-128-CBC", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r#"crypto\.createCipheriv\s*\(\s*['"]des-ede3-cbc['"]"#, "3DES", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r#"crypto\.createCipheriv\s*\(\s*['"]chacha20-poly1305['"]"#, "ChaCha20-Poly1305", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r#"crypto\.createCipheriv\s*\(\s*['"]rc4['"]"#, "RC4", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r#"crypto\.createCipheriv\s*\(\s*['"]bf-cbc['"]"#, "Blowfish", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),

        pat(r#"crypto\.createHash\s*\(\s*['"]sha256['"]"#, "SHA-256", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r#"crypto\.createHash\s*\(\s*['"]sha384['"]"#, "SHA-384", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r#"crypto\.createHash\s*\(\s*['"]sha512['"]"#, "SHA-512", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r#"crypto\.createHash\s*\(\s*['"]sha1['"]"#, "SHA-1", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r#"crypto\.createHash\s*\(\s*['"]md5['"]"#, "MD5", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),

        pat(r#"crypto\.createHmac\s*\(\s*['"]sha256['"]"#, "HMAC-SHA256", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r#"crypto\.createHmac\s*\(\s*['"]sha512['"]"#, "HMAC-SHA512", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r#"crypto\.createHmac\s*\(\s*['"]sha1['"]"#, "HMAC-SHA1", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),

        pat(r#"crypto\.generateKeyPairSync\s*\(\s*['"]rsa['"]"#, "RSA", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r#"crypto\.generateKeyPairSync\s*\(\s*['"]ec['"]"#, "ECDSA", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r#"crypto\.generateKeyPairSync\s*\(\s*['"]ed25519['"]"#, "Ed25519", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r#"crypto\.generateKeyPairSync\s*\(\s*['"]x25519['"]"#, "X25519", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),

        pat(r"crypto\.scryptSync\b|crypto\.scrypt\b", "scrypt", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r"crypto\.pbkdf2Sync\b|crypto\.pbkdf2\b", "PBKDF2", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r"crypto\.diffieHellman\b|crypto\.createDiffieHellman\b", "DH", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),
        pat(r"crypto\.createECDH\b", "ECDH", DetectionMethod::ApiCall, "node", &["js", "ts", "mjs"], Some("crypto"), Confidence::High),

        // -- require/import --
        pat(r#"require\s*\(\s*['"]crypto-js['"]\s*\)"#, "crypto-js", DetectionMethod::ImportStatement, "node", &["js", "ts", "mjs"], Some("crypto-js"), Confidence::High),
        pat(r#"from\s+['"]crypto-js['"]"#, "crypto-js", DetectionMethod::ImportStatement, "node", &["js", "ts", "mjs"], Some("crypto-js"), Confidence::High),
        pat(r#"require\s*\(\s*['"]bcryptjs?['"]\s*\)"#, "bcrypt", DetectionMethod::ImportStatement, "node", &["js", "ts", "mjs"], Some("bcrypt"), Confidence::High),
        pat(r#"from\s+['"]bcryptjs?['"]"#, "bcrypt", DetectionMethod::ImportStatement, "node", &["js", "ts", "mjs"], Some("bcrypt"), Confidence::High),
        pat(r#"require\s*\(\s*['"]argon2['"]\s*\)"#, "Argon2", DetectionMethod::ImportStatement, "node", &["js", "ts", "mjs"], Some("argon2"), Confidence::High),
        pat(r#"from\s+['"]argon2['"]"#, "Argon2", DetectionMethod::ImportStatement, "node", &["js", "ts", "mjs"], Some("argon2"), Confidence::High),
        pat(r#"require\s*\(\s*['"]node-forge['"]\s*\)"#, "node-forge", DetectionMethod::ImportStatement, "node", &["js", "ts", "mjs"], Some("node-forge"), Confidence::High),
        pat(r#"from\s+['"]jose['"]"#, "JOSE", DetectionMethod::ImportStatement, "node", &["js", "ts", "mjs"], Some("jose"), Confidence::High),

        // -- package.json --
        pat(r#""crypto-js""#, "crypto-js", DetectionMethod::DependencyManifest, "node", &["json"], None, Confidence::Medium),
        pat(r#""bcrypt""#, "bcrypt", DetectionMethod::DependencyManifest, "node", &["json"], None, Confidence::Medium),
        pat(r#""argon2""#, "Argon2", DetectionMethod::DependencyManifest, "node", &["json"], None, Confidence::Medium),
        pat(r#""node-forge""#, "node-forge", DetectionMethod::DependencyManifest, "node", &["json"], None, Confidence::Medium),
        pat(r#""jose""#, "JOSE", DetectionMethod::DependencyManifest, "node", &["json"], None, Confidence::Medium),
    ]
}

// ─── Rust Patterns ────────────────────────────────────────────────────

fn rust_patterns() -> Vec<CryptoPattern> {
    vec![
        // -- ring --
        pat(r"ring::aead::AES_256_GCM\b", "AES-256-GCM", DetectionMethod::ApiCall, "rust", &["rs"], Some("ring"), Confidence::High),
        pat(r"ring::aead::AES_128_GCM\b", "AES-128-GCM", DetectionMethod::ApiCall, "rust", &["rs"], Some("ring"), Confidence::High),
        pat(r"ring::aead::CHACHA20_POLY1305\b", "ChaCha20-Poly1305", DetectionMethod::ApiCall, "rust", &["rs"], Some("ring"), Confidence::High),
        pat(r"ring::digest::SHA256\b", "SHA-256", DetectionMethod::ApiCall, "rust", &["rs"], Some("ring"), Confidence::High),
        pat(r"ring::digest::SHA384\b", "SHA-384", DetectionMethod::ApiCall, "rust", &["rs"], Some("ring"), Confidence::High),
        pat(r"ring::digest::SHA512\b", "SHA-512", DetectionMethod::ApiCall, "rust", &["rs"], Some("ring"), Confidence::High),
        pat(r"ring::digest::SHA1_FOR_LEGACY_USE_ONLY\b", "SHA-1", DetectionMethod::ApiCall, "rust", &["rs"], Some("ring"), Confidence::High),
        pat(r"ring::hmac::", "HMAC", DetectionMethod::ApiCall, "rust", &["rs"], Some("ring"), Confidence::High),
        pat(r"ring::signature::RSA", "RSA", DetectionMethod::ApiCall, "rust", &["rs"], Some("ring"), Confidence::High),
        pat(r"ring::signature::ECDSA", "ECDSA", DetectionMethod::ApiCall, "rust", &["rs"], Some("ring"), Confidence::High),
        pat(r"ring::signature::ED25519\b", "Ed25519", DetectionMethod::ApiCall, "rust", &["rs"], Some("ring"), Confidence::High),
        pat(r"ring::agreement::X25519\b", "X25519", DetectionMethod::ApiCall, "rust", &["rs"], Some("ring"), Confidence::High),
        pat(r"ring::pbkdf2::", "PBKDF2", DetectionMethod::ApiCall, "rust", &["rs"], Some("ring"), Confidence::High),
        pat(r"ring::hkdf::", "HKDF", DetectionMethod::ApiCall, "rust", &["rs"], Some("ring"), Confidence::High),

        // -- RustCrypto --
        pat(r"use\s+aes::", "AES", DetectionMethod::ImportStatement, "rust", &["rs"], Some("aes"), Confidence::High),
        pat(r"use\s+aes_gcm::", "AES-GCM", DetectionMethod::ImportStatement, "rust", &["rs"], Some("aes-gcm"), Confidence::High),
        pat(r"use\s+chacha20poly1305::", "ChaCha20-Poly1305", DetectionMethod::ImportStatement, "rust", &["rs"], Some("chacha20poly1305"), Confidence::High),
        pat(r"use\s+sha2::", "SHA-256", DetectionMethod::ImportStatement, "rust", &["rs"], Some("sha2"), Confidence::High),
        pat(r"use\s+sha3::", "SHA-3", DetectionMethod::ImportStatement, "rust", &["rs"], Some("sha3"), Confidence::High),
        pat(r"use\s+blake2::", "BLAKE2", DetectionMethod::ImportStatement, "rust", &["rs"], Some("blake2"), Confidence::High),
        pat(r"use\s+blake3::", "BLAKE3", DetectionMethod::ImportStatement, "rust", &["rs"], Some("blake3"), Confidence::High),
        pat(r"use\s+md5::", "MD5", DetectionMethod::ImportStatement, "rust", &["rs"], Some("md-5"), Confidence::High),
        pat(r"use\s+rsa::", "RSA", DetectionMethod::ImportStatement, "rust", &["rs"], Some("rsa"), Confidence::High),
        pat(r"use\s+ed25519_dalek::", "Ed25519", DetectionMethod::ImportStatement, "rust", &["rs"], Some("ed25519-dalek"), Confidence::High),
        pat(r"use\s+x25519_dalek::", "X25519", DetectionMethod::ImportStatement, "rust", &["rs"], Some("x25519-dalek"), Confidence::High),
        pat(r"use\s+p256::", "ECDSA-P256", DetectionMethod::ImportStatement, "rust", &["rs"], Some("p256"), Confidence::High),
        pat(r"use\s+p384::", "ECDSA-P384", DetectionMethod::ImportStatement, "rust", &["rs"], Some("p384"), Confidence::High),
        pat(r"use\s+argon2::", "Argon2", DetectionMethod::ImportStatement, "rust", &["rs"], Some("argon2"), Confidence::High),
        pat(r"use\s+scrypt::", "scrypt", DetectionMethod::ImportStatement, "rust", &["rs"], Some("scrypt"), Confidence::High),
        pat(r"use\s+bcrypt::", "bcrypt", DetectionMethod::ImportStatement, "rust", &["rs"], Some("bcrypt"), Confidence::High),
        pat(r"use\s+hkdf::", "HKDF", DetectionMethod::ImportStatement, "rust", &["rs"], Some("hkdf"), Confidence::High),
        pat(r"use\s+pbkdf2::", "PBKDF2", DetectionMethod::ImportStatement, "rust", &["rs"], Some("pbkdf2"), Confidence::High),

        // -- Cargo.toml dependencies --
        pat(r#"(?m)^ring\s*="#, "ring", DetectionMethod::DependencyManifest, "rust", &["toml"], None, Confidence::High),
        pat(r#"(?m)^aes-gcm\s*="#, "AES-GCM", DetectionMethod::DependencyManifest, "rust", &["toml"], None, Confidence::High),
        pat(r#"(?m)^chacha20poly1305\s*="#, "ChaCha20-Poly1305", DetectionMethod::DependencyManifest, "rust", &["toml"], None, Confidence::High),
        pat(r#"(?m)^sha2\s*="#, "SHA-256", DetectionMethod::DependencyManifest, "rust", &["toml"], None, Confidence::Medium),
        pat(r#"(?m)^blake2\s*="#, "BLAKE2", DetectionMethod::DependencyManifest, "rust", &["toml"], None, Confidence::Medium),
        pat(r#"(?m)^blake3\s*="#, "BLAKE3", DetectionMethod::DependencyManifest, "rust", &["toml"], None, Confidence::Medium),
        pat(r#"(?m)^argon2\s*="#, "Argon2", DetectionMethod::DependencyManifest, "rust", &["toml"], None, Confidence::Medium),
        pat(r#"(?m)^rustls\s*="#, "TLS", DetectionMethod::DependencyManifest, "rust", &["toml"], None, Confidence::Medium),
        pat(r#"(?m)^openssl\s*="#, "OpenSSL", DetectionMethod::DependencyManifest, "rust", &["toml"], None, Confidence::Medium),
        pat(r#"(?m)^ed25519-dalek\s*="#, "Ed25519", DetectionMethod::DependencyManifest, "rust", &["toml"], None, Confidence::Medium),
        pat(r#"(?m)^x25519-dalek\s*="#, "X25519", DetectionMethod::DependencyManifest, "rust", &["toml"], None, Confidence::Medium),
    ]
}

// ─── Ruby Patterns ────────────────────────────────────────────────────

fn ruby_patterns() -> Vec<CryptoPattern> {
    vec![
        // -- OpenSSL (builtin) - Ciphers --
        pat(r#"OpenSSL::Cipher\.new\s*\(\s*['"]AES-256-GCM['"]"#, "AES-256-GCM", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r#"OpenSSL::Cipher\.new\s*\(\s*['"]AES-256-CBC['"]"#, "AES-256-CBC", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r#"OpenSSL::Cipher\.new\s*\(\s*['"]AES-128-GCM['"]"#, "AES-128-GCM", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r#"OpenSSL::Cipher\.new\s*\(\s*['"]AES-128-CBC['"]"#, "AES-128-CBC", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r#"OpenSSL::Cipher\.new\s*\(\s*['"]DES-EDE3-CBC['"]"#, "3DES", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r#"OpenSSL::Cipher\.new\s*\(\s*['"]DES['"]"#, "DES", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r#"OpenSSL::Cipher\.new\s*\(\s*['"]BF-CBC['"]"#, "Blowfish", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r#"OpenSSL::Cipher\.new\s*\(\s*['"]RC4['"]"#, "RC4", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r#"OpenSSL::Cipher\.new\s*\(\s*['"]ChaCha20['"]"#, "ChaCha20-Poly1305", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r"OpenSSL::Cipher\b", "AES", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::Low),

        // -- OpenSSL - Digests --
        pat(r#"OpenSSL::Digest\.new\s*\(\s*['"]SHA256['"]"#, "SHA-256", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r#"OpenSSL::Digest\.new\s*\(\s*['"]SHA384['"]"#, "SHA-384", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r#"OpenSSL::Digest\.new\s*\(\s*['"]SHA512['"]"#, "SHA-512", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r#"OpenSSL::Digest\.new\s*\(\s*['"]SHA1['"]"#, "SHA-1", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r#"OpenSSL::Digest\.new\s*\(\s*['"]MD5['"]"#, "MD5", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r"OpenSSL::Digest::SHA256\b", "SHA-256", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r"OpenSSL::Digest::SHA512\b", "SHA-512", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r"OpenSSL::Digest::SHA1\b", "SHA-1", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r"OpenSSL::Digest::MD5\b", "MD5", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r"Digest::SHA256\b", "SHA-256", DetectionMethod::ApiCall, "ruby", &["rb"], Some("digest"), Confidence::High),
        pat(r"Digest::SHA512\b", "SHA-512", DetectionMethod::ApiCall, "ruby", &["rb"], Some("digest"), Confidence::High),
        pat(r"Digest::SHA1\b", "SHA-1", DetectionMethod::ApiCall, "ruby", &["rb"], Some("digest"), Confidence::High),
        pat(r"Digest::MD5\b", "MD5", DetectionMethod::ApiCall, "ruby", &["rb"], Some("digest"), Confidence::High),

        // -- OpenSSL - Asymmetric --
        pat(r"OpenSSL::PKey::RSA\.new\b", "RSA", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r"OpenSSL::PKey::RSA\.generate\b", "RSA", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r"OpenSSL::PKey::EC\.new\b", "ECDSA", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r"OpenSSL::PKey::EC\.generate\b", "ECDSA", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),
        pat(r"OpenSSL::PKey::DH\b", "DH", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),

        // -- OpenSSL - HMAC --
        pat(r"OpenSSL::HMAC\b", "HMAC", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),

        // -- OpenSSL - PBKDF2 --
        pat(r"OpenSSL::PKCS5\.pbkdf2_hmac\b", "PBKDF2", DetectionMethod::ApiCall, "ruby", &["rb"], Some("openssl"), Confidence::High),

        // -- Imports --
        pat(r#"require\s+['"]openssl['"]"#, "OpenSSL", DetectionMethod::ImportStatement, "ruby", &["rb"], Some("openssl"), Confidence::Medium),
        pat(r#"require\s+['"]digest['"]"#, "SHA-256", DetectionMethod::ImportStatement, "ruby", &["rb"], Some("digest"), Confidence::Low),

        // -- rbnacl --
        pat(r"RbNaCl::SecretBox\b", "XSalsa20-Poly1305", DetectionMethod::ApiCall, "ruby", &["rb"], Some("rbnacl"), Confidence::High),
        pat(r"RbNaCl::SimpleBox\b", "X25519", DetectionMethod::ApiCall, "ruby", &["rb"], Some("rbnacl"), Confidence::High),
        pat(r"RbNaCl::AEAD::ChaCha20Poly1305\b", "ChaCha20-Poly1305", DetectionMethod::ApiCall, "ruby", &["rb"], Some("rbnacl"), Confidence::High),
        pat(r"RbNaCl::Hash\.sha256\b", "SHA-256", DetectionMethod::ApiCall, "ruby", &["rb"], Some("rbnacl"), Confidence::High),
        pat(r"RbNaCl::Hash\.sha512\b", "SHA-512", DetectionMethod::ApiCall, "ruby", &["rb"], Some("rbnacl"), Confidence::High),
        pat(r"RbNaCl::Hash\.blake2b\b", "BLAKE2b", DetectionMethod::ApiCall, "ruby", &["rb"], Some("rbnacl"), Confidence::High),
        pat(r"RbNaCl::Signatures::Ed25519\b", "Ed25519", DetectionMethod::ApiCall, "ruby", &["rb"], Some("rbnacl"), Confidence::High),
        pat(r"RbNaCl::GroupElements::Curve25519\b", "X25519", DetectionMethod::ApiCall, "ruby", &["rb"], Some("rbnacl"), Confidence::High),
        pat(r"RbNaCl::PasswordHash\.scrypt\b", "scrypt", DetectionMethod::ApiCall, "ruby", &["rb"], Some("rbnacl"), Confidence::High),
        pat(r"RbNaCl::PasswordHash\.argon2\b", "Argon2", DetectionMethod::ApiCall, "ruby", &["rb"], Some("rbnacl"), Confidence::High),

        // -- bcrypt-ruby --
        pat(r"BCrypt::Password\b", "bcrypt", DetectionMethod::ApiCall, "ruby", &["rb"], Some("bcrypt-ruby"), Confidence::High),
        pat(r#"require\s+['"]bcrypt['"]"#, "bcrypt", DetectionMethod::ImportStatement, "ruby", &["rb"], Some("bcrypt-ruby"), Confidence::High),

        // -- Dependency manifests (Gemfile) --
        pat(r#"gem\s+['"]rbnacl['"]\b"#, "rbnacl", DetectionMethod::DependencyManifest, "ruby", &["rb"], None, Confidence::Medium),
        pat(r#"gem\s+['"]bcrypt['"]\b"#, "bcrypt", DetectionMethod::DependencyManifest, "ruby", &["rb"], None, Confidence::Medium),
        pat(r#"gem\s+['"]ed25519['"]\b"#, "Ed25519", DetectionMethod::DependencyManifest, "ruby", &["rb"], None, Confidence::Medium),
        pat(r#"gem\s+['"]rsa['"]\b"#, "RSA", DetectionMethod::DependencyManifest, "ruby", &["rb"], None, Confidence::Medium),
    ]
}

// ─── C# / .NET Patterns ──────────────────────────────────────────────

fn csharp_patterns() -> Vec<CryptoPattern> {
    vec![
        // -- System.Security.Cryptography API calls --
        pat(r"Aes\.Create\b", "AES", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"AesGcm\b", "AES-256-GCM", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"AesCcm\b", "AES-CCM", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"TripleDES\.Create\b", "3DES", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"DES\.Create\b", "DES", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"RC2\.Create\b", "RC2", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),

        // -- Hash --
        pat(r"SHA256\.Create\b", "SHA-256", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"SHA384\.Create\b", "SHA-384", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"SHA512\.Create\b", "SHA-512", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"SHA1\.Create\b", "SHA-1", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"MD5\.Create\b", "MD5", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"SHA256\.HashData\b", "SHA-256", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"SHA512\.HashData\b", "SHA-512", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),

        // -- Asymmetric --
        pat(r"RSA\.Create\b", "RSA", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"RSACryptoServiceProvider\b", "RSA", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"ECDsa\.Create\b", "ECDSA", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"ECDiffieHellman\.Create\b", "ECDH", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"DSA\.Create\b", "DSA", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),

        // -- HMAC --
        pat(r"HMACSHA256\b", "HMAC-SHA256", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"HMACSHA512\b", "HMAC-SHA512", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"HMACSHA1\b", "HMAC-SHA1", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"HMACMD5\b", "HMAC-MD5", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),

        // -- KDF --
        pat(r"Rfc2898DeriveBytes\b", "PBKDF2", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"HKDF\.DeriveKey\b", "HKDF", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),
        pat(r"HKDF\.Extract\b", "HKDF", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::High),

        // -- RandomNumberGenerator --
        pat(r"RandomNumberGenerator\.Create\b", "DRBG", DetectionMethod::ApiCall, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::Medium),

        // -- Imports --
        pat(r"using\s+System\.Security\.Cryptography\b", "System.Security.Cryptography", DetectionMethod::ImportStatement, "csharp", &["cs"], Some("System.Security.Cryptography"), Confidence::Medium),
        pat(r"using\s+Org\.BouncyCastle\b", "BouncyCastle", DetectionMethod::ImportStatement, "csharp", &["cs"], Some("BouncyCastle"), Confidence::High),

        // -- BouncyCastle API calls --
        pat(r"AesFastEngine\b|AesEngine\b", "AES", DetectionMethod::ApiCall, "csharp", &["cs"], Some("BouncyCastle"), Confidence::High),
        pat(r"GcmBlockCipher\b", "AES-GCM", DetectionMethod::ApiCall, "csharp", &["cs"], Some("BouncyCastle"), Confidence::High),
        pat(r"RsaEngine\b|RsaKeyPairGenerator\b", "RSA", DetectionMethod::ApiCall, "csharp", &["cs"], Some("BouncyCastle"), Confidence::High),
        pat(r"ECKeyPairGenerator\b", "ECDSA", DetectionMethod::ApiCall, "csharp", &["cs"], Some("BouncyCastle"), Confidence::High),
        pat(r"Ed25519Signer\b|Ed25519KeyPairGenerator\b", "Ed25519", DetectionMethod::ApiCall, "csharp", &["cs"], Some("BouncyCastle"), Confidence::High),
        pat(r"X25519Agreement\b|X25519KeyPairGenerator\b", "X25519", DetectionMethod::ApiCall, "csharp", &["cs"], Some("BouncyCastle"), Confidence::High),
        pat(r"Sha256Digest\b", "SHA-256", DetectionMethod::ApiCall, "csharp", &["cs"], Some("BouncyCastle"), Confidence::High),
        pat(r"Sha512Digest\b", "SHA-512", DetectionMethod::ApiCall, "csharp", &["cs"], Some("BouncyCastle"), Confidence::High),
        pat(r"Sha1Digest\b", "SHA-1", DetectionMethod::ApiCall, "csharp", &["cs"], Some("BouncyCastle"), Confidence::High),
        pat(r"MD5Digest\b", "MD5", DetectionMethod::ApiCall, "csharp", &["cs"], Some("BouncyCastle"), Confidence::High),
        pat(r"ChaCha20Poly1305\b", "ChaCha20-Poly1305", DetectionMethod::ApiCall, "csharp", &["cs"], Some("BouncyCastle"), Confidence::High),
        pat(r"BlowfishEngine\b", "Blowfish", DetectionMethod::ApiCall, "csharp", &["cs"], Some("BouncyCastle"), Confidence::High),

        // -- Dependency manifests (.csproj) --
        pat(r"BouncyCastle\.Cryptography", "BouncyCastle", DetectionMethod::DependencyManifest, "csharp", &["csproj", "fsproj", "config", "props"], None, Confidence::High),
        pat(r"Portable\.BouncyCastle", "BouncyCastle", DetectionMethod::DependencyManifest, "csharp", &["csproj", "fsproj", "config", "props"], None, Confidence::High),
        pat(r"BCrypt\.Net-Next", "bcrypt", DetectionMethod::DependencyManifest, "csharp", &["csproj", "fsproj", "config", "props"], None, Confidence::Medium),
        pat(r"Konscious\.Security\.Cryptography", "Argon2", DetectionMethod::DependencyManifest, "csharp", &["csproj", "fsproj", "config", "props"], None, Confidence::Medium),
        pat(r"NSec\.Cryptography", "X25519", DetectionMethod::DependencyManifest, "csharp", &["csproj", "fsproj", "config", "props"], None, Confidence::Medium),
    ]
}

// ─── PHP Patterns ─────────────────────────────────────────────────────

fn php_patterns() -> Vec<CryptoPattern> {
    vec![
        // -- openssl extension --
        pat(r"openssl_encrypt\s*\(", "AES", DetectionMethod::ApiCall, "php", &["php"], Some("openssl"), Confidence::High),
        pat(r"openssl_decrypt\s*\(", "AES", DetectionMethod::ApiCall, "php", &["php"], Some("openssl"), Confidence::High),
        pat(r#"openssl_encrypt\s*\([^,]+,\s*['"]aes-256-gcm['"]"#, "AES-256-GCM", DetectionMethod::ApiCall, "php", &["php"], Some("openssl"), Confidence::High),
        pat(r#"openssl_encrypt\s*\([^,]+,\s*['"]aes-256-cbc['"]"#, "AES-256-CBC", DetectionMethod::ApiCall, "php", &["php"], Some("openssl"), Confidence::High),
        pat(r#"openssl_encrypt\s*\([^,]+,\s*['"]aes-128-gcm['"]"#, "AES-128-GCM", DetectionMethod::ApiCall, "php", &["php"], Some("openssl"), Confidence::High),
        pat(r#"openssl_encrypt\s*\([^,]+,\s*['"]aes-128-cbc['"]"#, "AES-128-CBC", DetectionMethod::ApiCall, "php", &["php"], Some("openssl"), Confidence::High),
        pat(r#"openssl_encrypt\s*\([^,]+,\s*['"]des-ede3-cbc['"]"#, "3DES", DetectionMethod::ApiCall, "php", &["php"], Some("openssl"), Confidence::High),
        pat(r#"openssl_encrypt\s*\([^,]+,\s*['"]bf-cbc['"]"#, "Blowfish", DetectionMethod::ApiCall, "php", &["php"], Some("openssl"), Confidence::High),
        pat(r#"openssl_encrypt\s*\([^,]+,\s*['"]rc4['"]"#, "RC4", DetectionMethod::ApiCall, "php", &["php"], Some("openssl"), Confidence::High),
        pat(r#"openssl_encrypt\s*\([^,]+,\s*['"]chacha20-poly1305['"]"#, "ChaCha20-Poly1305", DetectionMethod::ApiCall, "php", &["php"], Some("openssl"), Confidence::High),
        pat(r"openssl_digest\s*\(", "SHA-256", DetectionMethod::ApiCall, "php", &["php"], Some("openssl"), Confidence::Medium),
        pat(r"openssl_sign\s*\(", "RSA", DetectionMethod::ApiCall, "php", &["php"], Some("openssl"), Confidence::Medium),
        pat(r"openssl_verify\s*\(", "RSA", DetectionMethod::ApiCall, "php", &["php"], Some("openssl"), Confidence::Medium),
        pat(r"openssl_pkey_new\s*\(", "RSA", DetectionMethod::ApiCall, "php", &["php"], Some("openssl"), Confidence::Medium),
        pat(r"openssl_seal\s*\(", "RSA", DetectionMethod::ApiCall, "php", &["php"], Some("openssl"), Confidence::Medium),
        pat(r"openssl_pbkdf2\s*\(", "PBKDF2", DetectionMethod::ApiCall, "php", &["php"], Some("openssl"), Confidence::High),

        // -- hash extension --
        pat(r#"hash\s*\(\s*['"]sha256['"]"#, "SHA-256", DetectionMethod::ApiCall, "php", &["php"], Some("hash"), Confidence::High),
        pat(r#"hash\s*\(\s*['"]sha384['"]"#, "SHA-384", DetectionMethod::ApiCall, "php", &["php"], Some("hash"), Confidence::High),
        pat(r#"hash\s*\(\s*['"]sha512['"]"#, "SHA-512", DetectionMethod::ApiCall, "php", &["php"], Some("hash"), Confidence::High),
        pat(r#"hash\s*\(\s*['"]sha1['"]"#, "SHA-1", DetectionMethod::ApiCall, "php", &["php"], Some("hash"), Confidence::High),
        pat(r#"hash\s*\(\s*['"]md5['"]"#, "MD5", DetectionMethod::ApiCall, "php", &["php"], Some("hash"), Confidence::High),
        pat(r"\bmd5\s*\(", "MD5", DetectionMethod::ApiCall, "php", &["php"], Some("hash"), Confidence::High),
        pat(r"\bsha1\s*\(", "SHA-1", DetectionMethod::ApiCall, "php", &["php"], Some("hash"), Confidence::High),
        pat(r#"hash_hmac\s*\(\s*['"]sha256['"]"#, "HMAC-SHA256", DetectionMethod::ApiCall, "php", &["php"], Some("hash"), Confidence::High),
        pat(r#"hash_hmac\s*\(\s*['"]sha512['"]"#, "HMAC-SHA512", DetectionMethod::ApiCall, "php", &["php"], Some("hash"), Confidence::High),
        pat(r#"hash_hmac\s*\(\s*['"]sha1['"]"#, "HMAC-SHA1", DetectionMethod::ApiCall, "php", &["php"], Some("hash"), Confidence::High),
        pat(r"hash_pbkdf2\s*\(", "PBKDF2", DetectionMethod::ApiCall, "php", &["php"], Some("hash"), Confidence::High),

        // -- password_hash (bcrypt/argon2) --
        pat(r"password_hash\s*\(", "bcrypt", DetectionMethod::ApiCall, "php", &["php"], Some("password"), Confidence::Medium),
        pat(r"PASSWORD_BCRYPT\b", "bcrypt", DetectionMethod::ApiCall, "php", &["php"], Some("password"), Confidence::High),
        pat(r"PASSWORD_ARGON2I\b", "Argon2", DetectionMethod::ApiCall, "php", &["php"], Some("password"), Confidence::High),
        pat(r"PASSWORD_ARGON2ID\b", "Argon2", DetectionMethod::ApiCall, "php", &["php"], Some("password"), Confidence::High),

        // -- sodium extension --
        pat(r"sodium_crypto_secretbox\b", "XSalsa20-Poly1305", DetectionMethod::ApiCall, "php", &["php"], Some("sodium"), Confidence::High),
        pat(r"sodium_crypto_aead_chacha20poly1305\b", "ChaCha20-Poly1305", DetectionMethod::ApiCall, "php", &["php"], Some("sodium"), Confidence::High),
        pat(r"sodium_crypto_aead_aes256gcm\b", "AES-256-GCM", DetectionMethod::ApiCall, "php", &["php"], Some("sodium"), Confidence::High),
        pat(r"sodium_crypto_box\b", "X25519", DetectionMethod::ApiCall, "php", &["php"], Some("sodium"), Confidence::High),
        pat(r"sodium_crypto_sign\b", "Ed25519", DetectionMethod::ApiCall, "php", &["php"], Some("sodium"), Confidence::High),
        pat(r"sodium_crypto_generichash\b", "BLAKE2b", DetectionMethod::ApiCall, "php", &["php"], Some("sodium"), Confidence::High),
        pat(r"sodium_crypto_auth\b", "HMAC-SHA512", DetectionMethod::ApiCall, "php", &["php"], Some("sodium"), Confidence::High),
        pat(r"sodium_crypto_pwhash\b", "Argon2", DetectionMethod::ApiCall, "php", &["php"], Some("sodium"), Confidence::High),
        pat(r"sodium_crypto_kdf\b", "BLAKE2b", DetectionMethod::ApiCall, "php", &["php"], Some("sodium"), Confidence::High),
        pat(r"sodium_crypto_scalarmult\b", "X25519", DetectionMethod::ApiCall, "php", &["php"], Some("sodium"), Confidence::High),

        // -- phpseclib --
        pat(r"use\s+phpseclib3?\\Crypt\\AES\b", "AES", DetectionMethod::ImportStatement, "php", &["php"], Some("phpseclib"), Confidence::High),
        pat(r"use\s+phpseclib3?\\Crypt\\RSA\b", "RSA", DetectionMethod::ImportStatement, "php", &["php"], Some("phpseclib"), Confidence::High),
        pat(r"use\s+phpseclib3?\\Crypt\\DES\b", "DES", DetectionMethod::ImportStatement, "php", &["php"], Some("phpseclib"), Confidence::High),
        pat(r"use\s+phpseclib3?\\Crypt\\TripleDES\b", "3DES", DetectionMethod::ImportStatement, "php", &["php"], Some("phpseclib"), Confidence::High),
        pat(r"use\s+phpseclib3?\\Crypt\\Blowfish\b", "Blowfish", DetectionMethod::ImportStatement, "php", &["php"], Some("phpseclib"), Confidence::High),
        pat(r"use\s+phpseclib3?\\Crypt\\RC4\b", "RC4", DetectionMethod::ImportStatement, "php", &["php"], Some("phpseclib"), Confidence::High),
        pat(r"use\s+phpseclib3?\\Crypt\\EC\b", "ECDSA", DetectionMethod::ImportStatement, "php", &["php"], Some("phpseclib"), Confidence::High),
        pat(r"use\s+phpseclib3?\\Crypt\\Hash\b", "SHA-256", DetectionMethod::ImportStatement, "php", &["php"], Some("phpseclib"), Confidence::Medium),

        // -- Dependency manifests (composer.json) --
        pat(r#""phpseclib/phpseclib""#, "phpseclib", DetectionMethod::DependencyManifest, "php", &["json"], None, Confidence::High),
        pat(r#""defuse/php-encryption""#, "AES", DetectionMethod::DependencyManifest, "php", &["json"], None, Confidence::Medium),
        pat(r#""paragonie/halite""#, "XSalsa20-Poly1305", DetectionMethod::DependencyManifest, "php", &["json"], None, Confidence::Medium),
        pat(r#""paragonie/sodium_compat""#, "X25519", DetectionMethod::DependencyManifest, "php", &["json"], None, Confidence::Medium),
    ]
}

// ─── Swift Patterns ───────────────────────────────────────────────────

fn swift_patterns() -> Vec<CryptoPattern> {
    vec![
        // -- CryptoKit --
        pat(r"import\s+CryptoKit\b", "CryptoKit", DetectionMethod::ImportStatement, "swift", &["swift"], Some("CryptoKit"), Confidence::Medium),
        pat(r"AES\.GCM\.seal\b", "AES-256-GCM", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"AES\.GCM\.open\b", "AES-256-GCM", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"AES\.GCM\b", "AES-256-GCM", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"ChaChaPoly\.seal\b", "ChaCha20-Poly1305", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"ChaChaPoly\.open\b", "ChaCha20-Poly1305", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"ChaChaPoly\b", "ChaCha20-Poly1305", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"SHA256\.hash\b", "SHA-256", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"SHA384\.hash\b", "SHA-384", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"SHA512\.hash\b", "SHA-512", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"Insecure\.SHA1\.hash\b", "SHA-1", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"Insecure\.SHA1\b", "SHA-1", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"Insecure\.MD5\.hash\b", "MD5", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"Insecure\.MD5\b", "MD5", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"HMAC<SHA256>\b", "HMAC-SHA256", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"HMAC<SHA384>\b", "HMAC-SHA384", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"HMAC<SHA512>\b", "HMAC-SHA512", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"P256\.Signing\b", "ECDSA-P256", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"P256\.KeyAgreement\b", "ECDH-P256", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"P384\.Signing\b", "ECDSA-P384", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"P384\.KeyAgreement\b", "ECDH-P384", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"P521\.Signing\b", "ECDSA-P521", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"P521\.KeyAgreement\b", "ECDH-P521", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"Curve25519\.Signing\b", "Ed25519", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"Curve25519\.KeyAgreement\b", "X25519", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"HKDF<SHA256>\b|HKDF<SHA512>\b", "HKDF", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::High),
        pat(r"SymmetricKey\b", "AES", DetectionMethod::ApiCall, "swift", &["swift"], Some("CryptoKit"), Confidence::Low),

        // -- CommonCrypto --
        pat(r"import\s+CommonCrypto\b", "CommonCrypto", DetectionMethod::ImportStatement, "swift", &["swift"], Some("CommonCrypto"), Confidence::Medium),
        pat(r"CCCrypt\b", "AES", DetectionMethod::ApiCall, "swift", &["swift"], Some("CommonCrypto"), Confidence::High),
        pat(r"CCCryptorCreate\b", "AES", DetectionMethod::ApiCall, "swift", &["swift"], Some("CommonCrypto"), Confidence::High),
        pat(r"kCCAlgorithmAES\b", "AES", DetectionMethod::ApiCall, "swift", &["swift"], Some("CommonCrypto"), Confidence::High),
        pat(r"kCCAlgorithmDES\b", "DES", DetectionMethod::ApiCall, "swift", &["swift"], Some("CommonCrypto"), Confidence::High),
        pat(r"kCCAlgorithm3DES\b", "3DES", DetectionMethod::ApiCall, "swift", &["swift"], Some("CommonCrypto"), Confidence::High),
        pat(r"kCCAlgorithmBlowfish\b", "Blowfish", DetectionMethod::ApiCall, "swift", &["swift"], Some("CommonCrypto"), Confidence::High),
        pat(r"kCCAlgorithmRC4\b", "RC4", DetectionMethod::ApiCall, "swift", &["swift"], Some("CommonCrypto"), Confidence::High),
        pat(r"CC_SHA256\b", "SHA-256", DetectionMethod::ApiCall, "swift", &["swift"], Some("CommonCrypto"), Confidence::High),
        pat(r"CC_SHA384\b", "SHA-384", DetectionMethod::ApiCall, "swift", &["swift"], Some("CommonCrypto"), Confidence::High),
        pat(r"CC_SHA512\b", "SHA-512", DetectionMethod::ApiCall, "swift", &["swift"], Some("CommonCrypto"), Confidence::High),
        pat(r"CC_SHA1\b", "SHA-1", DetectionMethod::ApiCall, "swift", &["swift"], Some("CommonCrypto"), Confidence::High),
        pat(r"CC_MD5\b", "MD5", DetectionMethod::ApiCall, "swift", &["swift"], Some("CommonCrypto"), Confidence::High),
        pat(r"CCHmac\b", "HMAC", DetectionMethod::ApiCall, "swift", &["swift"], Some("CommonCrypto"), Confidence::High),
        pat(r"CCKeyDerivationPBKDF\b", "PBKDF2", DetectionMethod::ApiCall, "swift", &["swift"], Some("CommonCrypto"), Confidence::High),

        // -- Dependency manifests --
        pat(r#"\.package\s*\(\s*url:.*CryptoSwift"#, "CryptoSwift", DetectionMethod::DependencyManifest, "swift", &["swift"], None, Confidence::High),
        pat(r#"\.package\s*\(\s*url:.*swift-crypto"#, "swift-crypto", DetectionMethod::DependencyManifest, "swift", &["swift"], None, Confidence::High),
        pat(r#"pod\s+['"]CryptoSwift['"]\b"#, "CryptoSwift", DetectionMethod::DependencyManifest, "swift", &["rb"], None, Confidence::High),
        pat(r#"pod\s+['"]OpenSSL['"]\b"#, "OpenSSL", DetectionMethod::DependencyManifest, "swift", &["rb"], None, Confidence::High),
        pat(r#"pod\s+['"]RNCryptor['"]\b"#, "AES", DetectionMethod::DependencyManifest, "swift", &["rb"], None, Confidence::Medium),
    ]
}

// ─── Elixir Patterns ──────────────────────────────────────────────────

fn elixir_patterns() -> Vec<CryptoPattern> {
    vec![
        // -- :crypto (Erlang) - Symmetric --
        pat(r":crypto\.crypto_one_time\b", "AES", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":crypto\.crypto_one_time_aead\b", "AES-GCM", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":crypto\.block_encrypt\b", "AES", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":crypto\.block_decrypt\b", "AES", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":crypto\.stream_encrypt\b", "AES-CTR", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":aes_256_gcm\b", "AES-256-GCM", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":aes_128_gcm\b", "AES-128-GCM", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":aes_256_cbc\b", "AES-256-CBC", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":aes_128_cbc\b", "AES-128-CBC", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":aes_ecb\b", "AES-ECB", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":chacha20_poly1305\b", "ChaCha20-Poly1305", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":des_ede3_cbc\b", "3DES", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":blowfish_cbc\b", "Blowfish", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":rc4\b", "RC4", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),

        // -- :crypto - Hash --
        pat(r":crypto\.hash\s*\(\s*:sha256\b", "SHA-256", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":crypto\.hash\s*\(\s*:sha384\b", "SHA-384", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":crypto\.hash\s*\(\s*:sha512\b", "SHA-512", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":crypto\.hash\s*\(\s*:sha\b", "SHA-1", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":crypto\.hash\s*\(\s*:md5\b", "MD5", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":crypto\.hash\s*\(\s*:sha3_256\b", "SHA-3", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":crypto\.hash\s*\(\s*:blake2b\b", "BLAKE2b", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),

        // -- :crypto - MAC --
        pat(r":crypto\.mac\s*\(\s*:hmac\b", "HMAC", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":crypto\.macN?\s*\(\s*:cmac\b", "CMAC", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),

        // -- :crypto - Asymmetric --
        pat(r":crypto\.generate_key\s*\(\s*:rsa\b", "RSA", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":crypto\.generate_key\s*\(\s*:ecdh\b", "ECDH", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":crypto\.generate_key\s*\(\s*:eddsa\b", "Ed25519", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::High),
        pat(r":crypto\.sign\b", "RSA", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::Medium),
        pat(r":crypto\.verify\b", "RSA", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":crypto"), Confidence::Medium),

        // -- :public_key --
        pat(r":public_key\.sign\b", "RSA", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":public_key"), Confidence::Medium),
        pat(r":public_key\.verify\b", "RSA", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":public_key"), Confidence::Medium),
        pat(r":public_key\.encrypt_private\b", "RSA", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":public_key"), Confidence::High),
        pat(r":public_key\.encrypt_public\b", "RSA", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some(":public_key"), Confidence::High),

        // -- Comeonin / Bcrypt / Argon2 --
        pat(r"Bcrypt\.hash_pwd_salt\b", "bcrypt", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some("bcrypt_elixir"), Confidence::High),
        pat(r"Bcrypt\.verify_pass\b", "bcrypt", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some("bcrypt_elixir"), Confidence::High),
        pat(r"Bcrypt\.no_user_verify\b", "bcrypt", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some("bcrypt_elixir"), Confidence::High),
        pat(r"Argon2\.hash_pwd_salt\b", "Argon2", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some("argon2_elixir"), Confidence::High),
        pat(r"Argon2\.verify_pass\b", "Argon2", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some("argon2_elixir"), Confidence::High),
        pat(r"Pbkdf2\.hash_pwd_salt\b", "PBKDF2", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some("pbkdf2_elixir"), Confidence::High),
        pat(r"Comeonin\.Bcrypt\b", "bcrypt", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some("comeonin"), Confidence::High),
        pat(r"Comeonin\.Argon2\b", "Argon2", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some("comeonin"), Confidence::High),
        pat(r"Comeonin\.Pbkdf2\b", "PBKDF2", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some("comeonin"), Confidence::High),

        // -- Plug.Crypto / JOSE --
        pat(r"Plug\.Crypto\b", "AES", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some("plug_crypto"), Confidence::Medium),
        pat(r"JOSE\.JWK\b", "JOSE", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some("jose"), Confidence::High),
        pat(r"JOSE\.JWS\b", "JOSE", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some("jose"), Confidence::High),
        pat(r"JOSE\.JWE\b", "JOSE", DetectionMethod::ApiCall, "elixir", &["ex", "exs"], Some("jose"), Confidence::High),

        // -- Dependency manifests (mix.exs) --
        pat(r#"\{:bcrypt_elixir\b"#, "bcrypt", DetectionMethod::DependencyManifest, "elixir", &["exs"], None, Confidence::High),
        pat(r#"\{:argon2_elixir\b"#, "Argon2", DetectionMethod::DependencyManifest, "elixir", &["exs"], None, Confidence::High),
        pat(r#"\{:pbkdf2_elixir\b"#, "PBKDF2", DetectionMethod::DependencyManifest, "elixir", &["exs"], None, Confidence::High),
        pat(r#"\{:comeonin\b"#, "bcrypt", DetectionMethod::DependencyManifest, "elixir", &["exs"], None, Confidence::Medium),
        pat(r#"\{:jose\b"#, "JOSE", DetectionMethod::DependencyManifest, "elixir", &["exs"], None, Confidence::Medium),
        pat(r#"\{:plug_crypto\b"#, "AES", DetectionMethod::DependencyManifest, "elixir", &["exs"], None, Confidence::Medium),
        pat(r#"\{:ex_crypto\b"#, "AES", DetectionMethod::DependencyManifest, "elixir", &["exs"], None, Confidence::Medium),
    ]
}

// ─── Cross-ecosystem patterns (config files, etc.) ────────────────────

fn cross_ecosystem_patterns() -> Vec<CryptoPattern> {
    vec![
        // TLS configuration
        pat(r"TLSv1\.3", "TLS-1.3", DetectionMethod::ConfigFile, "any", &["conf", "cfg", "yaml", "yml", "toml", "json", "xml", "ini", "properties"], None, Confidence::Medium),
        pat(r"TLSv1\.2", "TLS-1.2", DetectionMethod::ConfigFile, "any", &["conf", "cfg", "yaml", "yml", "toml", "json", "xml", "ini", "properties"], None, Confidence::Medium),
        pat(r"TLSv1\.1", "TLS-1.1", DetectionMethod::ConfigFile, "any", &["conf", "cfg", "yaml", "yml", "toml", "json", "xml", "ini", "properties"], None, Confidence::Medium),
        pat(r"SSLv3", "SSL-3.0", DetectionMethod::ConfigFile, "any", &["conf", "cfg", "yaml", "yml", "toml", "json", "xml", "ini", "properties"], None, Confidence::Medium),

        // OpenSSL config
        pat(r"fips\s*=\s*fips_sect", "FIPS-mode", DetectionMethod::ConfigFile, "any", &["cnf", "conf", "cfg"], None, Confidence::High),
    ]
}

// ─── Helper ───────────────────────────────────────────────────────────

fn pat(
    regex: &str,
    name: &str,
    method: DetectionMethod,
    ecosystem: &'static str,
    extensions: &[&'static str],
    library: Option<&str>,
    confidence: Confidence,
) -> CryptoPattern {
    CryptoPattern {
        regex: Regex::new(regex).unwrap_or_else(|e| panic!("Invalid regex '{regex}': {e}")),
        algorithm_name: name.to_string(),
        detection_method: method,
        ecosystem,
        file_extensions: extensions.to_vec(),
        providing_library: library.map(|s| s.to_string()),
        confidence,
    }
}

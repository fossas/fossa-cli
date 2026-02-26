use serde::{Deserialize, Serialize};

/// FIPS compliance status for a cryptographic algorithm.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "kebab-case")]
pub enum FipsStatus {
    /// Fully FIPS-approved
    Approved,
    /// FIPS-approved but deprecated (e.g., SHA-1 for non-signature, 3DES decrypt-only)
    Deprecated,
    /// Not FIPS-approved
    NotApproved,
}

impl FipsStatus {
    pub fn is_approved(&self) -> bool {
        matches!(self, FipsStatus::Approved)
    }

    pub fn label(&self) -> &str {
        match self {
            FipsStatus::Approved => "FIPS Approved",
            FipsStatus::Deprecated => "FIPS Deprecated",
            FipsStatus::NotApproved => "Not FIPS Approved",
        }
    }
}

/// FIPS remediation suggestion.
#[allow(dead_code)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FipsRemediation {
    pub algorithm: String,
    pub status: FipsStatus,
    pub reason: String,
    pub recommended_alternative: Option<String>,
}

/// Classify an algorithm name to its FIPS status using our comprehensive database.
pub fn classify_algorithm(name: &str) -> (FipsStatus, Option<String>) {
    let lower = name.to_lowercase();

    // --- Symmetric Encryption ---
    if lower.contains("aes") {
        if lower.contains("ecb") {
            return (
                FipsStatus::Deprecated,
                Some("AES-ECB mode is deprecated; use AES-GCM or AES-CBC".into()),
            );
        }
        return (FipsStatus::Approved, None);
    }

    if lower.contains("chacha20") || lower.contains("chacha") {
        return (
            FipsStatus::NotApproved,
            Some("Replace with AES-256-GCM".into()),
        );
    }

    if lower.contains("3des") || lower.contains("triple") && lower.contains("des") || lower.contains("desede") || lower.contains("tdea") {
        return (
            FipsStatus::Deprecated,
            Some("3DES is legacy-only since Jan 2024; migrate to AES".into()),
        );
    }

    if lower.contains("blowfish") || lower == "bf" || lower.contains("bf-cbc") {
        return (
            FipsStatus::NotApproved,
            Some("Replace with AES".into()),
        );
    }

    if lower.contains("rc4") || lower.contains("arcfour") || lower.contains("arc4") {
        return (
            FipsStatus::NotApproved,
            Some("Replace with AES-GCM".into()),
        );
    }

    if lower == "des" || lower == "des-cbc" || lower == "des-ede" {
        return (
            FipsStatus::NotApproved,
            Some("DES is insecure; replace with AES".into()),
        );
    }

    if lower.contains("salsa20") || lower.contains("xsalsa") {
        return (
            FipsStatus::NotApproved,
            Some("Replace with AES-256-GCM".into()),
        );
    }

    if lower.contains("camellia") || lower.contains("twofish") || lower.contains("cast5")
        || lower.contains("seed") || lower.contains("aria") || lower.contains("sm4")
        || lower.contains("idea")
    {
        return (FipsStatus::NotApproved, Some("Replace with AES".into()));
    }

    // --- Hash Functions ---
    if lower.contains("sha3") || lower.contains("sha-3") {
        return (FipsStatus::Approved, None);
    }

    if lower.contains("sha256") || lower.contains("sha-256") || lower.contains("sha_256") {
        return (FipsStatus::Approved, None);
    }

    if lower.contains("sha384") || lower.contains("sha-384") || lower.contains("sha_384") {
        return (FipsStatus::Approved, None);
    }

    if lower.contains("sha512") || lower.contains("sha-512") || lower.contains("sha_512") {
        return (FipsStatus::Approved, None);
    }

    if lower.contains("sha224") || lower.contains("sha-224") {
        return (
            FipsStatus::Deprecated,
            Some("SHA-224 will be disallowed by 2030; use SHA-256+".into()),
        );
    }

    if lower.contains("sha1") || lower.contains("sha-1") || lower == "sha" {
        return (
            FipsStatus::Deprecated,
            Some("SHA-1 is deprecated; use SHA-256 or SHA-3".into()),
        );
    }

    if lower.contains("shake128") || lower.contains("shake256") {
        return (FipsStatus::Approved, None);
    }

    if lower.contains("md5") {
        return (
            FipsStatus::NotApproved,
            Some("MD5 is not FIPS-approved; use SHA-256".into()),
        );
    }

    if lower.contains("md4") {
        return (
            FipsStatus::NotApproved,
            Some("MD4 is broken; use SHA-256".into()),
        );
    }

    if lower.contains("blake2") || lower.contains("blake3") {
        return (
            FipsStatus::NotApproved,
            Some("Replace with SHA-256 or SHA-3".into()),
        );
    }

    if lower.contains("ripemd") || lower.contains("whirlpool") || lower.contains("sm3") {
        return (
            FipsStatus::NotApproved,
            Some("Replace with SHA-256 or SHA-3".into()),
        );
    }

    // --- Asymmetric / Signatures ---
    if lower.contains("rsa") {
        // Extract key size from algorithm name (e.g., "rsa-1536", "rsa-2048")
        let key_size = lower
            .split(|c: char| !c.is_ascii_digit())
            .find_map(|tok| tok.parse::<u32>().ok());

        if let Some(bits) = key_size {
            if bits < 2048 {
                return (
                    FipsStatus::NotApproved,
                    Some("RSA key size below 2048 bits; use RSA-2048 or higher".into()),
                );
            }
        }
        return (FipsStatus::Approved, None);
    }

    if lower.contains("ecdsa") {
        return (FipsStatus::Approved, None);
    }

    if lower.contains("ed25519") || lower.contains("ed448") || lower.contains("eddsa") {
        return (FipsStatus::Approved, None); // Approved for signatures per FIPS 186-5
    }

    if lower.contains("dsa") && !lower.contains("ecdsa") && !lower.contains("eddsa") && !lower.contains("ml-dsa") {
        return (
            FipsStatus::Deprecated,
            Some("DSA is deprecated; only verification allowed. Use ECDSA or EdDSA".into()),
        );
    }

    // --- Key Exchange ---
    if lower.contains("ecdh") {
        if lower.contains("x25519") || lower.contains("curve25519") {
            return (
                FipsStatus::NotApproved,
                Some("X25519 is not FIPS-approved for key exchange; use ECDH P-256/P-384".into()),
            );
        }
        return (FipsStatus::Approved, None);
    }

    if lower.contains("x25519") || lower.contains("curve25519") {
        return (
            FipsStatus::NotApproved,
            Some("X25519/Curve25519 is not FIPS-approved; use ECDH with NIST curves".into()),
        );
    }

    if lower.contains("x448") {
        return (
            FipsStatus::NotApproved,
            Some("X448 is not FIPS-approved; use ECDH P-384 or P-521".into()),
        );
    }

    if lower.contains("diffie") || lower == "dh" {
        return (FipsStatus::Approved, None); // Assuming >= 2048-bit
    }

    // --- Post-Quantum ---
    if lower.contains("ml-kem") || lower.contains("mlkem") || lower.contains("kyber") {
        return (FipsStatus::Approved, None);
    }

    if lower.contains("ml-dsa") || lower.contains("mldsa") || lower.contains("dilithium") {
        return (FipsStatus::Approved, None);
    }

    if lower.contains("slh-dsa") || lower.contains("slhdsa") || lower.contains("sphincs") {
        return (FipsStatus::Approved, None);
    }

    // --- MACs ---
    if lower.contains("hmac") {
        return (FipsStatus::Approved, None);
    }

    if lower.contains("cmac") || lower.contains("gmac") || lower.contains("kmac") {
        return (FipsStatus::Approved, None);
    }

    if lower.contains("poly1305") {
        return (
            FipsStatus::NotApproved,
            Some("Poly1305 is not FIPS-approved; use HMAC or CMAC".into()),
        );
    }

    if lower.contains("siphash") {
        return (
            FipsStatus::NotApproved,
            Some("SipHash is not FIPS-approved; use HMAC".into()),
        );
    }

    // --- KDFs ---
    if lower.contains("hkdf") || lower.contains("pbkdf2") {
        return (FipsStatus::Approved, None);
    }

    if lower.contains("argon2") {
        return (
            FipsStatus::NotApproved,
            Some("Argon2 is not FIPS-approved; use PBKDF2".into()),
        );
    }

    if lower.contains("scrypt") {
        return (
            FipsStatus::NotApproved,
            Some("scrypt is not FIPS-approved; use PBKDF2".into()),
        );
    }

    if lower.contains("bcrypt") {
        return (
            FipsStatus::NotApproved,
            Some("bcrypt is not FIPS-approved; use PBKDF2".into()),
        );
    }

    // --- DRBGs ---
    if lower.contains("drbg") {
        if lower.contains("dual_ec") || lower.contains("dual-ec") {
            return (
                FipsStatus::NotApproved,
                Some("Dual_EC_DRBG was removed; use Hash_DRBG, HMAC_DRBG, or CTR_DRBG".into()),
            );
        }
        return (FipsStatus::Approved, None);
    }

    // Default: unknown algorithms are flagged for review
    (FipsStatus::NotApproved, Some("Unknown algorithm; verify FIPS status manually".into()))
}

use serde::{Deserialize, Serialize};

use crate::fips::FipsStatus;

/// A detected cryptographic algorithm with all its metadata.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CryptoAlgorithm {
    /// Canonical name (e.g., "AES-256-GCM", "SHA-256", "ChaCha20-Poly1305")
    pub name: String,
    /// CycloneDX 1.7 algorithm family from the registry
    pub algorithm_family: String,
    /// Cryptographic primitive type
    pub primitive: Primitive,
    /// Parameter set identifier (e.g., "256" for AES-256, "2048" for RSA-2048)
    pub parameter_set: Option<String>,
    /// Elliptic curve (for ECC algorithms)
    pub elliptic_curve: Option<String>,
    /// Block cipher mode (for symmetric ciphers)
    pub mode: Option<String>,
    /// OID if known
    pub oid: Option<String>,
    /// Classical security level in bits
    pub classical_security_level: Option<u32>,
    /// NIST quantum security level (0 = not quantum safe, 1-5)
    pub nist_quantum_security_level: u8,
    /// FIPS compliance status
    pub fips_status: FipsStatus,
    /// Crypto functions performed
    pub crypto_functions: Vec<String>,
}

/// Cryptographic primitive type (maps to CycloneDX 1.7 enum).
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "kebab-case")]
pub enum Primitive {
    Ae,
    BlockCipher,
    StreamCipher,
    Hash,
    Mac,
    Signature,
    Pke,
    Kem,
    KeyAgree,
    Kdf,
    Xof,
    Drbg,
    Combiner,
    Other,
    Unknown,
}

/// A single crypto finding: an algorithm detected at a specific location.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CryptoFinding {
    /// The detected algorithm
    pub algorithm: CryptoAlgorithm,
    /// Source file where detected
    pub file_path: String,
    /// Line number (0 if unknown)
    pub line_number: usize,
    /// The matched text snippet
    pub matched_text: String,
    /// Detection method
    pub detection_method: DetectionMethod,
    /// Ecosystem in which it was found
    pub ecosystem: String,
    /// The library providing this algorithm (if known)
    pub providing_library: Option<String>,
    /// Confidence level
    pub confidence: Confidence,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "kebab-case")]
pub enum DetectionMethod {
    DependencyManifest,
    ImportStatement,
    ApiCall,
    ConfigFile,
    StringLiteral,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum Confidence {
    High,
    Medium,
    Low,
}

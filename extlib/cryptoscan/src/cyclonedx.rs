use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use uuid::Uuid;

use crate::crypto_algorithm::CryptoFinding;

/// CycloneDX 1.7 BOM structure for CBOM output.
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CycloneDxBom {
    pub bom_format: String,
    pub spec_version: String,
    pub serial_number: String,
    pub version: u32,
    pub metadata: BomMetadata,
    pub components: Vec<BomComponent>,
    pub dependencies: Vec<BomDependency>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BomMetadata {
    pub timestamp: String,
    pub tools: Vec<BomTool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub component: Option<BomMetadataComponent>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct BomTool {
    pub name: String,
    pub version: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct BomMetadataComponent {
    #[serde(rename = "type")]
    pub component_type: String,
    pub name: String,
    pub version: String,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BomComponent {
    #[serde(rename = "type")]
    pub component_type: String,
    #[serde(rename = "bom-ref")]
    pub bom_ref: String,
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub crypto_properties: Option<CryptoProperties>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<Vec<BomProperty>>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CryptoProperties {
    pub asset_type: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub algorithm_properties: Option<AlgorithmProperties>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub oid: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AlgorithmProperties {
    pub primitive: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub algorithm_family: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameter_set_identifier: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub elliptic_curve: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mode: Option<String>,
    pub execution_environment: String,
    pub implementation_platform: String,
    pub certification_level: Vec<String>,
    pub crypto_functions: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub classical_security_level: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nist_quantum_security_level: Option<u8>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct BomProperty {
    pub name: String,
    pub value: String,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BomDependency {
    #[serde(rename = "ref")]
    pub dep_ref: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub depends_on: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub provides: Option<Vec<String>>,
}

/// Convert a list of crypto findings into a CycloneDX 1.7 BOM.
pub fn to_cyclonedx_bom(findings: &[CryptoFinding]) -> CycloneDxBom {
    let mut components = Vec::new();
    let mut dependencies = Vec::new();
    // Key by (ecosystem, library_name) to avoid collapsing libraries across ecosystems
    let mut library_algorithms: BTreeMap<(String, String), Vec<String>> = BTreeMap::new();

    // Group findings by bom_ref to aggregate detection contexts
    let mut algo_findings: BTreeMap<String, Vec<&CryptoFinding>> = BTreeMap::new();
    for finding in findings {
        let bom_ref = make_bom_ref(&finding.algorithm.name, &finding.algorithm.oid);

        // Track library -> algorithm for `provides` relationships
        if let Some(lib) = &finding.providing_library {
            library_algorithms
                .entry((finding.ecosystem.clone(), lib.clone()))
                .or_default()
                .push(bom_ref.clone());
        }

        algo_findings.entry(bom_ref).or_default().push(finding);
    }

    // Create cryptographic-asset components with aggregated detection contexts
    for (bom_ref, group) in &algo_findings {
        let first = group[0];

        let primitive_str = first.algorithm.primitive.as_str().to_string();

        let fips_label = first.algorithm.fips_status.label().to_string();

        let algo_props = AlgorithmProperties {
            primitive: primitive_str,
            algorithm_family: valid_algorithm_family(&first.algorithm.algorithm_family),
            parameter_set_identifier: first.algorithm.parameter_set.clone(),
            elliptic_curve: first.algorithm.elliptic_curve.clone(),
            mode: first.algorithm.mode.clone(),
            execution_environment: "software-plain-ram".to_string(),
            implementation_platform: "generic".to_string(),
            certification_level: vec!["none".to_string()],
            crypto_functions: first.algorithm.crypto_functions.clone(),
            classical_security_level: first.algorithm.classical_security_level,
            nist_quantum_security_level: if first.algorithm.nist_quantum_security_level > 0 {
                Some(first.algorithm.nist_quantum_security_level)
            } else {
                None
            },
        };

        // Aggregate detection contexts from all findings for this algorithm
        let mut properties = vec![BomProperty {
            name: "fossa:fips-status".to_string(),
            value: fips_label,
        }];

        // Collect unique detection locations, methods, and ecosystems
        let mut seen_locations: BTreeSet<String> = BTreeSet::new();
        let mut seen_methods: BTreeSet<String> = BTreeSet::new();
        let mut seen_ecosystems: BTreeSet<String> = BTreeSet::new();

        for finding in group {
            seen_locations.insert(finding.file_path.clone());
            seen_methods.insert(finding.detection_method.as_str().to_string());
            seen_ecosystems.insert(finding.ecosystem.clone());
        }

        for location in &seen_locations {
            properties.push(BomProperty {
                name: "fossa:detected-in".to_string(),
                value: location.clone(),
            });
        }
        for method in &seen_methods {
            properties.push(BomProperty {
                name: "fossa:detection-method".to_string(),
                value: method.clone(),
            });
        }
        for ecosystem in &seen_ecosystems {
            properties.push(BomProperty {
                name: "fossa:ecosystem".to_string(),
                value: ecosystem.clone(),
            });
        }

        let component = BomComponent {
            component_type: "cryptographic-asset".to_string(),
            bom_ref: bom_ref.clone(),
            name: first.algorithm.name.clone(),
            crypto_properties: Some(CryptoProperties {
                asset_type: "algorithm".to_string(),
                algorithm_properties: Some(algo_props),
                oid: first.algorithm.oid.clone(),
            }),
            properties: Some(properties),
        };

        components.push(component);

        // Algorithm dependency entry
        dependencies.push(BomDependency {
            dep_ref: bom_ref.clone(),
            depends_on: None,
            provides: None,
        });
    }

    // Create library components with `provides` relationships
    for ((ecosystem, lib_name), algo_refs) in &library_algorithms {
        let lib_ref = format!("lib/{}/{}", ecosystem, lib_name);

        components.push(BomComponent {
            component_type: "library".to_string(),
            bom_ref: lib_ref.clone(),
            name: lib_name.clone(),
            crypto_properties: None,
            properties: None,
        });

        let unique_algos: Vec<String> = algo_refs
            .iter()
            .cloned()
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect();

        dependencies.push(BomDependency {
            dep_ref: lib_ref,
            depends_on: None,
            provides: Some(unique_algos),
        });
    }

    let now = chrono_timestamp();

    CycloneDxBom {
        bom_format: "CycloneDX".to_string(),
        spec_version: "1.7".to_string(),
        serial_number: format!("urn:uuid:{}", Uuid::new_v4()),
        version: 1,
        metadata: BomMetadata {
            timestamp: now,
            tools: vec![BomTool {
                name: "fossa-cryptoscan".to_string(),
                version: env!("CARGO_PKG_VERSION").to_string(),
            }],
            component: None,
        },
        components,
        dependencies,
    }
}

fn make_bom_ref(name: &str, oid: &Option<String>) -> String {
    let sanitized = name.to_lowercase().replace([' ', '/'], "-");
    match oid {
        Some(o) => format!("crypto/algorithm/{}@{}", sanitized, o),
        None => format!("crypto/algorithm/{}", sanitized),
    }
}

/// Validate an algorithm family string against the official CycloneDX 1.7 enum.
/// Returns `Some(canonical_form)` if matched, `None` otherwise.
fn valid_algorithm_family(family: &str) -> Option<String> {
    // Official CycloneDX 1.7 algorithmFamily enum values (case-sensitive in schema)
    const CANONICAL_FAMILIES: &[&str] = &[
        "3DES",
        "3GPP-XOR",
        "A5/1",
        "A5/2",
        "AES",
        "ARIA",
        "Ascon",
        "BLAKE2",
        "BLAKE3",
        "BLS",
        "Blowfish",
        "CAMELLIA",
        "CAST5",
        "CAST6",
        "CMAC",
        "CMEA",
        "ChaCha",
        "ChaCha20",
        "DES",
        "DSA",
        "ECDH",
        "ECDSA",
        "ECIES",
        "EdDSA",
        "ElGamal",
        "FFDH",
        "Fortuna",
        "GOST",
        "HC",
        "HKDF",
        "HMAC",
        "IDEA",
        "IKE-PRF",
        "KMAC",
        "LMS",
        "MD2",
        "MD4",
        "MD5",
        "MILENAGE",
        "ML-DSA",
        "ML-KEM",
        "MQV",
        "PBES1",
        "PBES2",
        "PBKDF1",
        "PBKDF2",
        "PBMAC1",
        "Poly1305",
        "RABBIT",
        "RC2",
        "RC4",
        "RC5",
        "RC6",
        "RIPEMD",
        "RSAES-OAEP",
        "RSAES-PKCS1",
        "RSASSA-PKCS1",
        "RSASSA-PSS",
        "SEED",
        "SHA-1",
        "SHA-2",
        "SHA-3",
        "SLH-DSA",
        "SNOW3G",
        "SP800-108",
        "Salsa20",
        "Serpent",
        "SipHash",
        "Skipjack",
        "TUAK",
        "Twofish",
        "Whirlpool",
        "X3DH",
        "XMSS",
        "Yarrow",
        "ZUC",
        "bcrypt",
    ];
    CANONICAL_FAMILIES
        .iter()
        .find(|&&canonical| canonical.eq_ignore_ascii_case(family))
        .map(|&canonical| canonical.to_string())
}

fn chrono_timestamp() -> String {
    // Simple UTC timestamp without chrono dependency
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();
    // Format as ISO 8601 (approximate)
    let days = now / 86400;
    let rem = now % 86400;
    let hours = rem / 3600;
    let mins = (rem % 3600) / 60;
    let secs = rem % 60;

    // Calculate year/month/day from days since epoch
    let (year, month, day) = days_to_date(days);
    format!(
        "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}Z",
        year, month, day, hours, mins, secs
    )
}

fn days_to_date(days_since_epoch: u64) -> (u64, u64, u64) {
    // Simplified date calculation
    let mut days = days_since_epoch as i64;
    let mut year = 1970i64;

    loop {
        let days_in_year = if is_leap(year) { 366 } else { 365 };
        if days < days_in_year {
            break;
        }
        days -= days_in_year;
        year += 1;
    }

    let months_days: Vec<i64> = if is_leap(year) {
        vec![31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    } else {
        vec![31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    };

    let mut month = 0;
    for (i, &md) in months_days.iter().enumerate() {
        if days < md {
            month = i + 1;
            break;
        }
        days -= md;
    }
    // Invariant: month is always set by the loop because days < days_in_year
    // after the year calculation, so at least one iteration will satisfy days < md.
    debug_assert!(month > 0, "days_to_date: month calculation failed");

    (year as u64, month as u64, (days + 1) as u64)
}

fn is_leap(year: i64) -> bool {
    (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
}

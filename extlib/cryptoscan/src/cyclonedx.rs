use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
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
    let mut seen_algorithms: HashSet<String> = HashSet::new();
    let mut library_algorithms: HashMap<String, Vec<String>> = HashMap::new();

    // Create cryptographic-asset components for each unique algorithm
    for finding in findings {
        let bom_ref = make_bom_ref(&finding.algorithm.name, &finding.algorithm.oid);

        if seen_algorithms.contains(&bom_ref) {
            // Still track the library -> algorithm relationship
            if let Some(lib) = &finding.providing_library {
                library_algorithms
                    .entry(lib.clone())
                    .or_default()
                    .push(bom_ref.clone());
            }
            continue;
        }
        seen_algorithms.insert(bom_ref.clone());

        let primitive_str = serde_json::to_value(&finding.algorithm.primitive)
            .ok()
            .and_then(|v| v.as_str().map(|s| s.to_string()))
            .unwrap_or_else(|| "unknown".to_string());

        let fips_label = finding.algorithm.fips_status.label().to_string();

        let algo_props = AlgorithmProperties {
            primitive: primitive_str,
            algorithm_family: Some(finding.algorithm.algorithm_family.clone()),
            parameter_set_identifier: finding.algorithm.parameter_set.clone(),
            elliptic_curve: finding.algorithm.elliptic_curve.clone(),
            mode: finding.algorithm.mode.clone(),
            execution_environment: "software-plain-ram".to_string(),
            implementation_platform: "generic".to_string(),
            certification_level: vec!["none".to_string()],
            crypto_functions: finding.algorithm.crypto_functions.clone(),
            classical_security_level: finding.algorithm.classical_security_level,
            nist_quantum_security_level: if finding.algorithm.nist_quantum_security_level > 0 {
                Some(finding.algorithm.nist_quantum_security_level)
            } else {
                None
            },
        };

        let component = BomComponent {
            component_type: "cryptographic-asset".to_string(),
            bom_ref: bom_ref.clone(),
            name: finding.algorithm.name.clone(),
            crypto_properties: Some(CryptoProperties {
                asset_type: "algorithm".to_string(),
                algorithm_properties: Some(algo_props),
                oid: finding.algorithm.oid.clone(),
            }),
            properties: Some(vec![
                BomProperty {
                    name: "fossa:fips-status".to_string(),
                    value: fips_label,
                },
                BomProperty {
                    name: "fossa:detected-in".to_string(),
                    value: finding.file_path.clone(),
                },
                BomProperty {
                    name: "fossa:detection-method".to_string(),
                    value: serde_json::to_value(&finding.detection_method)
                        .ok()
                        .and_then(|v| v.as_str().map(|s| s.to_string()))
                        .unwrap_or_default(),
                },
                BomProperty {
                    name: "fossa:ecosystem".to_string(),
                    value: finding.ecosystem.clone(),
                },
            ]),
        };

        components.push(component);

        // Track library -> algorithm for `provides` relationships
        if let Some(lib) = &finding.providing_library {
            library_algorithms
                .entry(lib.clone())
                .or_default()
                .push(bom_ref.clone());
        }

        // Algorithm dependency entry
        dependencies.push(BomDependency {
            dep_ref: bom_ref,
            depends_on: None,
            provides: None,
        });
    }

    // Create library components with `provides` relationships
    let mut seen_libs: HashSet<String> = HashSet::new();
    for (lib_name, algo_refs) in &library_algorithms {
        if seen_libs.contains(lib_name) {
            continue;
        }
        seen_libs.insert(lib_name.clone());

        let lib_ref = format!("lib/{}", lib_name);

        components.push(BomComponent {
            component_type: "library".to_string(),
            bom_ref: lib_ref.clone(),
            name: lib_name.clone(),
            crypto_properties: None,
            properties: None,
        });

        let unique_algos: Vec<String> = algo_refs
            .iter()
            .collect::<HashSet<_>>()
            .into_iter()
            .cloned()
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
    let sanitized = name
        .to_lowercase()
        .replace(' ', "-")
        .replace('/', "-");
    match oid {
        Some(o) => format!("crypto/algorithm/{}@{}", sanitized, o),
        None => format!("crypto/algorithm/{}", sanitized),
    }
}

fn chrono_timestamp() -> String {
    // Simple UTC timestamp without chrono dependency
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
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
    if month == 0 {
        month = 12;
    }

    (year as u64, month as u64, (days + 1) as u64)
}

fn is_leap(year: i64) -> bool {
    (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
}

use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{BufWriter, Read},
    path::{Path, PathBuf},
};

use clap::Parser;
use fingerprint::Combined;
use getset::Getters;
use serde::{Deserialize, Serialize};
use stable_eyre::{eyre::Context, Result};
use tar::{Archive, Entry};
use tracing::{debug, info, info_span, warn};
use typed_builder::TypedBuilder;

#[derive(Debug, Parser, Getters)]
#[getset(get = "pub")]
#[clap(version)]
pub struct Subcommand {
    /// The tar file image to search and fingerprint jars in.
    image_tar_file: PathBuf,
}

const JAR_OBSERVATION: &str = "v1.discover.binary.jar";

#[derive(Debug, PartialEq, Eq, Serialize, Clone)]
struct DiscoveredJar {
    kind: &'static str,
    path: PathBuf,
    fingerprints: Combined,
}

impl DiscoveredJar {
    fn new(path: PathBuf, fingerprints: Combined) -> Self {
        DiscoveredJar {
            kind: JAR_OBSERVATION,
            path,
            fingerprints,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
struct OciManifest {
    #[serde(rename = "Layers")]
    layers: Vec<PathBuf>,
}

/// The path in the manifest file corresponding to a layer.
#[derive(Debug, PartialEq, Eq, Serialize, Hash)]
struct LayerPath(PathBuf);

#[derive(Debug, PartialEq, Eq, Serialize, TypedBuilder)]
struct JarAnalysis {
    /// Jars and fingerprints associated with each layer in a jar file.
    discovered_jars: HashMap<LayerPath, Vec<DiscoveredJar>>,
}

#[tracing::instrument]
pub fn main(opts: Subcommand) -> Result<()> {
    let tar_filename = opts.image_tar_file();
    let jar_analysis = jars_in_container(opts.image_tar_file())
        .with_context(|| format!("analyze container: {:?}", tar_filename))?;

    let mut stdout = BufWriter::new(std::io::stdout());
    serde_json::to_writer(&mut stdout, &jar_analysis).context("Serialize Results")
}

/// Extracts the container (saved via `docker save`) and finds JAR files inside any layer.
/// For each one found, fingerprints it and reports all those fingerprints along with their
/// layer and path.
#[tracing::instrument]
fn jars_in_container(image_path: &PathBuf) -> Result<JarAnalysis> {
    // Visit each layer and fingerprint the JARs within.
    info!("inspecting container");
    let layers = list_container_layers(image_path)?;
    let mut discoveries = HashMap::new();

    let mut image = unpack(image_path)?;
    for entry in image.entries().context("iterate entries")? {
        let entry = entry.context("read entry")?;
        let path = entry.path().context("read path")?;
        if !layers.contains(path.as_ref()) {
            debug!(?path, "skipped: not a layer file");
            continue;
        }

        let layer = path.to_path_buf();
        // Layers should have a form like blob
        let layer_discoveries =
            jars_in_layer(entry).with_context(|| format!("read layer '{layer:?}'"))?;
        discoveries.insert(LayerPath(layer), layer_discoveries);
    }

    Ok(JarAnalysis {
        discovered_jars: discoveries,
    })
}

/// Open and unpack a file and put it into a tar.
/// This is done repeatedly because `entries()` can only be read once from an Archive.
#[tracing::instrument]
fn unpack(path: &PathBuf) -> Result<Archive<File>> {
    let file = File::open(path).context("open tar file")?;
    Ok(tar::Archive::new(file))
}

#[tracing::instrument(skip(entry))]
fn jars_in_layer(entry: Entry<'_, impl Read>) -> Result<Vec<DiscoveredJar>> {
    let mut discoveries = Vec::new();

    let mut entry_archive = tar::Archive::new(entry);
    for entry in entry_archive.entries().context("list entries in layer")? {
        let entry = entry.context("read entry")?;
        let path = entry.path().context("read path")?;
        if !path.to_string_lossy().ends_with(".jar") {
            debug!(?path, "skipped: not a jar file");
            continue;
        }

        let path = path.to_path_buf();

        info_span!("jar", ?path).in_scope(|| -> Result<()> {
            debug!("fingerprinting");
            let entry = buffer(entry).context("read jar file")?;

            match Combined::from_buffer(entry.clone()) {
                Ok(fingerprints) => {
                    discoveries.push(DiscoveredJar::new(path.clone(), fingerprints))
                }
                Err(e) => warn!("failed to fingerprint: {e:?}"),
            }
            let mut discovered_in_jars =
                recursive_jars_in_jars(&entry, path, 0).context("recursively discover jars")?;
            discoveries.append(&mut discovered_in_jars);

            Ok(())
        })?;
    }

    Ok(discoveries)
}

const MAX_JAR_DEPTH: u32 = 100;

#[tracing::instrument(skip(jar_contents))]
fn recursive_jars_in_jars(
    jar_contents: &[u8],
    containing_jar_path: PathBuf,
    depth: u32,
) -> Result<Vec<DiscoveredJar>> {
    if depth > MAX_JAR_DEPTH {
        return Ok(vec![]);
    }
    let mut discoveries = Vec::new();
    // If the jar is a symlink we find empty contents and run into an error when trying to unzip it.
    // Due to this, we have decided to warn instead of error and skip the jar.
    let mut archive = match zip::ZipArchive::new(std::io::Cursor::new(jar_contents)) {
        Ok(archive) => archive,
        Err(e) => {
            warn!("failed to unzip jar: {e:?}");
            return Ok(vec![]);
        }
    };
    for path in archive.clone().file_names() {
        debug!("file_name: {path}");
        if !path.ends_with(".jar") {
            continue;
        }

        debug!(?path, "jar file found");
        let mut zip_file = archive
            .by_name(path)
            .context("getting zip file info by path")?;
        if !zip_file.is_file() {
            debug!(?path, "skipped: not a file");
            continue;
        }
        let mut buffer: Vec<u8> = Vec::new();
        zip_file
            .read_to_end(&mut buffer)
            .context("reading jar from zip into buffer")?;
        let joined_path = Path::new(&containing_jar_path).join(path);

        // fingerprint the jar
        match Combined::from_buffer(buffer.clone()) {
            Ok(fingerprints) => {
                discoveries.push(DiscoveredJar::new(joined_path.clone(), fingerprints))
            }
            Err(e) => warn!("failed to fingerprint: {e:?}"),
        }

        // recursively find more jars
        let mut discovered_in_jars = recursive_jars_in_jars(&buffer, joined_path, depth + 1)
            .context("recursively discover jars")?;
        discoveries.append(&mut discovered_in_jars);
    }
    Ok(discoveries)
}

#[tracing::instrument]
fn list_container_layers(layer_path: &PathBuf) -> Result<HashSet<PathBuf>> {
    let mut layers = HashSet::new();

    let mut container = unpack(layer_path)?;
    for entry in container.entries().context("list entries")? {
        let entry = match entry {
            Ok(entry) => entry,
            Err(e) => {
                warn!("failed to read entry: {e:?}");
                continue;
            }
        };

        let path = match entry.path() {
            Ok(path) => path,
            Err(e) => {
                warn!("Failed to read entry path: {e:?}");
                continue;
            }
        };

        if !path.ends_with("manifest.json") {
            debug!(?path, "skipped: not a manifest file");
            continue;
        }

        info!(?path, "extracting manifests for image");
        let manifests: Vec<OciManifest> = serde_json::from_reader(entry)
            .with_context(|| format!("parse manifest: {layer_path:?}"))?;

        for manifest in manifests {
            layers.extend(manifest.layers);
        }

        // There's only one manifest file.
        break;
    }

    Ok(layers)
}

#[tracing::instrument(skip(reader))]
fn buffer(mut reader: impl Read) -> Result<Vec<u8>> {
    let mut buf = Vec::new();
    reader.read_to_end(&mut buf).context("Read buffer")?;
    Ok(buf)
}

#[cfg(test)]
mod tests {
    use serde_json::Value;
    use tap::Pipe;

    use super::*;

    const MILLHONE_OUT: &str = r#"
{
  "discovered_jars": {
    "blobs/sha256/61aed1a8baa251dee118b9ab203c1e420f0eda0a9b3f9322d67d235dd27a12ee": [
      {
        "kind": "v1.discover.binary.jar",
        "path": "jackson-annotations-2.17.1.jar",
        "fingerprints": {
          "sha_256": "/MrYLhMXLA5DhNtxV3IZybhjHAgg9LGNqqVwFvtmHHY=",
          "v1.mavencentral.jar": "/KfvYZLJrQXQe8UNqZG/k3qErzo=",
          "v1.class.jar": "t2Btr6rNrvzghM5Nud2uldRGVjw0/n5rK9j0xooQQyk=",
          "v1.raw.jar": "wjGJk8cvY4tpKcUC5r8YuO15Wfv+rVuyWANBYCUIeDs="
        }
      }
    ],
    "blobs/sha256/0e4613a3c620a37d93aca05039001fb5a6063c9d9cfb0935e3aa984025f31198": [
      {
        "kind": "v1.discover.binary.jar",
        "path": "slf4j-ext-2.0.0.jar",
        "fingerprints": {
          "v1.mavencentral.jar": "WO8bdGURkfUQyqK6rJ4miP9caEU=",
          "v1.class.jar": "PexFkKDUkwq7Do2Pt3HVPjMBRfqj/Zzp+nK5D6LfPF4=",
          "sha_256": "bWERAhXZlaGR2AxgVJDRTAlbOtHqLIVOpmncfLIUKj0=",
          "v1.raw.jar": "7CEXDIU3h2Vcj6lmy4gbuh4KsMVCNgUZTCj9VA4VoV8="
        }
      }
    ],
    "blobs/sha256/632e84390ad558f9db0524f5e38a0af3e79c623a46bdce8a5e6a1761041b9850": [],
    "blobs/sha256/054f94aa7ce72b59cd6abac5462f77f0645b2f1a7b17e55d8f847a6da58c90db": [
      {
        "kind": "v1.discover.binary.jar",
        "path": "inner_directory/commons-email2-jakarta-2.0.0-M1.jar",
        "fingerprints": {
          "v1.class.jar": "2wRGbMGyGRwEXqNm53h1YK8OO879kvzDxazmJXiAcfI=",
          "v1.raw.jar": "QA4SAurtJeo+lx1Vqve5uQYnvDKLFx1NgsSuoWKi8pw=",
          "sha_256": "MuEcK3nOFuySTTg4HOJi3vvTpI9bYspfMHa9AK2merQ=",
          "v1.mavencentral.jar": "6bzpyKql6Q+UxKQgm14pcP4wHGo="
        }
      }
    ]
  }
}
"#;

    #[test]
    fn it_finds_expected_output() {
        let image_tar_file =
            PathBuf::from("../../test/App/Fossa/Container/testdata/jar_test_container.tar");

        let res = jars_in_container(&image_tar_file)
            .expect("Read jars out of container image.")
            .pipe(serde_json::to_value)
            .expect("encode as json");

        let expected: Value = serde_json::from_str(MILLHONE_OUT).expect("Parse expected json");
        pretty_assertions::assert_eq!(expected, res);
    }

    // This container contains top.jar which contains middle.jar, which contains deepest.jar
    // It also includes middle.jar and deepest.jar
    // So we should find 6 total jars: three from top.jar and its nested jars, two from middle.jar and its nested jar and then deepest.jar
    // We are also testing that the fingerprints from the nested jars are equal to the fingerprints when they are at top-level
    // See test/App/Fossa/Container/testdata/nested-jar/README.md for info on how nested_jars.tar was made
    #[test]
    fn it_finds_nested_jars() {
        let nested_jars_millhone_out: String = format!(
            r#"
        {{
          "discovered_jars": {{
            "blobs/sha256/3af1c7e331a4b6791c25101e0c862125a597d8d75d786aead62de19f78a5a992": [
              {{
                "kind": "v1.discover.binary.jar",
                "path": "jars/deepest.jar",
                "fingerprints": {{
                  "sha_256": "LsXfP24XYFIZnkS3Z7RaNim1o8/TtGnueThkZv9hCok=",
                  "v1.class.jar": "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=",
                  "v1.mavencentral.jar": "1+4xPh5QS5IW0H6lfbxamjtVVdk=",
                  "v1.raw.jar": "UMQ1yS7xM6tF4YMvAWz8UP6+qAIRq3JauBoiTlVUNkM="
                }}
              }}
            ],
            "blobs/sha256/5ee98bff2cf0e70d115677fc37f734d26848435eef5fe52e905229ff7a7d87fb": [
              {{
                "kind": "v1.discover.binary.jar",
                "path": "jars/middle.jar",
                "fingerprints": {{
                  "sha_256": "nKFXVngFtkHIv4FC/rr5o4k+v/KSKzWJ0B9uBuRb+4k=",
                  "v1.class.jar": "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=",
                  "v1.mavencentral.jar": "2XA3GFJJkvvpEbAM9nLnAypojEo=",
                  "v1.raw.jar": "36i3JNvrLMWCMfjB2c9bjQt4Vhmvfq29cb+Hqrb6XeI="
                }}
              }},
              {{
                "kind": "v1.discover.binary.jar",
                "path": "jars/middle.jar{separator}deepest.jar",
                "fingerprints": {{
                  "v1.mavencentral.jar": "1+4xPh5QS5IW0H6lfbxamjtVVdk=",
                  "sha_256": "LsXfP24XYFIZnkS3Z7RaNim1o8/TtGnueThkZv9hCok=",
                  "v1.raw.jar": "UMQ1yS7xM6tF4YMvAWz8UP6+qAIRq3JauBoiTlVUNkM=",
                  "v1.class.jar": "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
                }}
              }}
            ],
            "blobs/sha256/6979b741102e5c5c787f94ad8bfdebeee561b1b89f21139d38489e1b3d6f9096": [],
            "blobs/sha256/931c525b52485e01ab5e2926a4b3c884f1c7325782dca13bd11e345f46cc34c3": [],
            "blobs/sha256/10bb0e91eb016af401369ecaadccfea9f4768776e54d46ad4e9a0309c82f1d7f": [
              {{
                "kind": "v1.discover.binary.jar",
                "path": "jars/top.jar",
                "fingerprints": {{
                  "v1.raw.jar": "TNW7ezd3fqw3MULVTrexg68Q1x2PTDGk2DkltAqUefk=",
                  "v1.mavencentral.jar": "TtwsgEXwLd/8UFTohsFhJqYMJ74=",
                  "v1.class.jar": "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=",
                  "sha_256": "l9XTA5PwWJhnFlz9t0SWKvr2cHDmcytIVvPsr6vqFis="
                }}
              }},
              {{
                "kind": "v1.discover.binary.jar",
                "path": "jars/top.jar{separator}middle.jar",
                "fingerprints": {{
                  "v1.mavencentral.jar": "2XA3GFJJkvvpEbAM9nLnAypojEo=",
                  "v1.class.jar": "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=",
                  "v1.raw.jar": "36i3JNvrLMWCMfjB2c9bjQt4Vhmvfq29cb+Hqrb6XeI=",
                  "sha_256": "nKFXVngFtkHIv4FC/rr5o4k+v/KSKzWJ0B9uBuRb+4k="
                }}
              }},
              {{
                "kind": "v1.discover.binary.jar",
                "path": "jars/top.jar{separator}middle.jar{separator}deepest.jar",
                "fingerprints": {{
                  "v1.raw.jar": "UMQ1yS7xM6tF4YMvAWz8UP6+qAIRq3JauBoiTlVUNkM=",
                  "sha_256": "LsXfP24XYFIZnkS3Z7RaNim1o8/TtGnueThkZv9hCok=",
                  "v1.mavencentral.jar": "1+4xPh5QS5IW0H6lfbxamjtVVdk=",
                  "v1.class.jar": "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
                }}
              }}
            ]
          }}
        }}
        "#,
            separator = std::path::MAIN_SEPARATOR_STR.replace("\\", "\\\\")
        );
        let image_tar_file =
            PathBuf::from("../../test/App/Fossa/Container/testdata/nested_jars.tar");
        let res = jars_in_container(&image_tar_file)
            .expect("Read jars out of container image.")
            .pipe(serde_json::to_value)
            .expect("encode as json");
        let expected: Value =
            serde_json::from_str(&nested_jars_millhone_out).expect("Parse expected json");
        pretty_assertions::assert_eq!(expected, res);
    }
}

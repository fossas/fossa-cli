use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{BufWriter, Read},
    path::PathBuf,
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

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone)]
struct DiscoveredJar {
    kind: String,
    path: PathBuf,
    fingerprints: Combined,
}

impl DiscoveredJar {
    fn new(path: PathBuf, fingerprints: Combined) -> Self {
        DiscoveredJar {
            kind: JAR_OBSERVATION.to_owned(),
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
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
struct LayerPath(PathBuf);

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, TypedBuilder)]
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

            match Combined::from_buffer(entry) {
                Ok(fingerprints) => discoveries.push(DiscoveredJar::new(path, fingerprints)),
                Err(e) => warn!("failed to fingerprint: {e:?}"),
            }

            Ok(())
        })?;
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
          "v1.raw.jar": "wjGJk8cvY4tpKcUC5r8YuO15Wfv+rVuyWANBYCUIeDs=",
          "v1.class.jar": "t2Btr6rNrvzghM5Nud2uldRGVjw0/n5rK9j0xooQQyk=",
          "sha_256": "/MrYLhMXLA5DhNtxV3IZybhjHAgg9LGNqqVwFvtmHHY=",
          "v1.mavencentral.jar": "/KfvYZLJrQXQe8UNqZG/k3qErzo="
        }
      }
    ],
    "blobs/sha256/4d84019f77d1aa837a2cb4225bb79fc03a45ae5a247284dda07cfb9fb8077bd1": [
      {
        "kind": "v1.discover.binary.jar",
        "path": "inner_directory/commons-email2-jakarta-2.0.0-M1.jar",
        "fingerprints": {
          "sha_256": "MuEcK3nOFuySTTg4HOJi3vvTpI9bYspfMHa9AK2merQ=",
          "v1.mavencentral.jar": "6bzpyKql6Q+UxKQgm14pcP4wHGo=",
          "v1.class.jar": "2wRGbMGyGRwEXqNm53h1YK8OO879kvzDxazmJXiAcfI=",
          "v1.raw.jar": "QA4SAurtJeo+lx1Vqve5uQYnvDKLFx1NgsSuoWKi8pw="
        }
      }
    ],
    "blobs/sha256/cc2447e1835a40530975ab80bb1f872fbab0f2a0faecf2ab16fbbb89b3589438": []
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
}

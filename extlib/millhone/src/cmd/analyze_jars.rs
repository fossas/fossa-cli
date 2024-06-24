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
    let jar_analysis = JarAnalysis {
        discovered_jars: jars_in_container(opts.image_tar_file())
            .with_context(|| format!("Analyze Jar Archive {:?}", tar_filename))?,
    };

    let mut stdout = BufWriter::new(std::io::stdout());
    serde_json::to_writer(&mut stdout, &jar_analysis).context("Serialize Results")
}

/// Extracts the container (saved via `docker save`) and finds JAR files inside any layer.
/// For each one found, fingerprints it and reports all those fingerprints along with their
/// layer and path.
#[tracing::instrument]
fn jars_in_container(image_path: &PathBuf) -> Result<HashMap<LayerPath, Vec<DiscoveredJar>>> {
    // Visit each layer and fingerprint the JARs within.
    info!("inspecting container");
    // May have to make two copies of this Archive, idk if it can be iterated twice.
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

    Ok(discoveries)
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
            let entry = buffer(entry).context("Read jar file")?;

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
                warn!("Entry path {:?}", e);
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

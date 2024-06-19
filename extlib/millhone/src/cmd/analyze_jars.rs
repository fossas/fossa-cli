use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::Read,
    path::PathBuf,
};

use clap::Parser;
use fingerprint::Combined;
use getset::Getters;
use serde::{Deserialize, Serialize};
use stable_eyre::{eyre::Context, Report};
use tar::{Archive, Entry};
use thiserror::Error;
use tracing::{debug, info, info_span, warn};
use typed_builder::TypedBuilder;

/// Tar errors are just [`std::io::Error`].
/// This macro is a bit cleaner to wrap them and adds a little context.
macro_rules! wrap_tar_err {
    ($tar_operation_str:expr, $operation:expr) => {
        $operation.map_err(|e| Error::ReadTar($tar_operation_str.to_string(), e))
    };
}

#[derive(Debug, Parser, Getters)]
#[getset(get = "pub")]
#[clap(version)]
pub struct Subcommand {
    /// The tar file image to search and fingerprint JARs in.
    image_tar_file: PathBuf,
}

#[derive(Debug, PartialEq, Eq, Serialize, Clone)]
struct DiscoveredJar {
    path: PathBuf,
    fingerprint: Combined,
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
struct OciManifest {
    #[serde(rename = "Layers")]
    layers: Vec<PathBuf>,
}

#[derive(Debug, PartialEq, Eq, Serialize, TypedBuilder)]
struct JarAnalysis {
    // A vector of the Jars discovered during an analysis.
    discovered_jars: HashMap<PathBuf, Vec<DiscoveredJar>>,
}

#[derive(Debug, Error)]
pub enum Error {
    /// Encountered when we can't open the requisite file.
    #[error("Open file {0}")]
    OpenFile(std::io::Error),

    /// Encountered when an archive operation fails.
    #[error("Tar operation '{0}': {1}")]
    ReadTar(String, std::io::Error),

    /// Encountered when a manifest file is found but cannot be parsed.
    #[error("Parsing manifest file {0} {1}")]
    ParseManifest(PathBuf, serde_json::Error),

    #[error("Reading buffer {0}")]
    ReadBuffer(std::io::Error),

    #[error("Generating fingerprint {0}")]
    GenerateFingerprint(fingerprint::Error),
}

#[tracing::instrument]
pub fn main(opts: Subcommand) -> Result<(), Report> {
    let tar_filename = opts.image_tar_file();
    let jar_analysis = JarAnalysis {
        discovered_jars: jars_in_container(opts.image_tar_file())
            .with_context(|| format!("Analyze Jar Archive {:?}", tar_filename))?,
    };

    serde_json::to_writer(std::io::stdout(), &jar_analysis).context("Serialize Results")
}

/// Extracts the container (saved via `docker save`) and finds JAR files inside any layer.
/// For each one found, fingerprints it and reports all those fingerprints along with their
/// layer and path.
#[tracing::instrument]
fn jars_in_container(image_path: &PathBuf) -> Result<HashMap<PathBuf, Vec<DiscoveredJar>>, Error> {
    // Visit each layer and fingerprint the JARs within.
    info!("inspecting container");
    // May have to make two copies of this Archive, idk if it can be iterated twice.
    let layers = list_container_layers(image_path)?;
    let mut discoveries = HashMap::new();

    let mut image_archive = unpack(image_path)?;
    for entry in wrap_tar_err!("Iterate entries", image_archive.entries())? {
        let entry = wrap_tar_err!("Read entry", entry)?;
        let path = wrap_tar_err!("Read path", entry.path())?;
        if !layers.contains(path.as_ref()) {
            debug!(?path, "skipped: not a layer file");
            continue;
        }

        let layer = path.to_path_buf();
        // Layers should have a form like blob
        let layer_discoveries = jars_in_layer(entry)?;
        discoveries.insert(layer, layer_discoveries);
    }

    Ok(discoveries)
}

/// Open and unpack a file and put it into a tar.
/// This is done repeatedly because `entries()` can only be read once from an Archive.
#[tracing::instrument]
fn unpack(path: &PathBuf) -> Result<Archive<File>, Error> {
    let file = File::open(path).map_err(Error::OpenFile)?;
    Ok(tar::Archive::new(file))
}

#[tracing::instrument(skip(entry))]
fn jars_in_layer(entry: Entry<'_, impl Read>) -> Result<Vec<DiscoveredJar>, Error> {
    let mut discoveries = Vec::new();

    let mut entry_archive = tar::Archive::new(entry);
    for entry in wrap_tar_err!("list entries in layer", entry_archive.entries())? {
        let entry = wrap_tar_err!("read entry", entry)?;
        let path = wrap_tar_err!("read path", entry.path())?;
        if !path.to_string_lossy().ends_with(".jar") {
            debug!(?path, "skipped: not a jar file");
            continue;
        }

        let path = path.to_path_buf();
        let res = info_span!("jar", ?path).in_scope(|| {
            debug!("fingerprinting");
            let entry = buffer(entry)?;

            match Combined::from_buffer(entry) {
                Ok(fingerprint) => {
                    discoveries.push(DiscoveredJar { fingerprint, path });
                    Ok(())
                }
                Err(e) => Err(Error::GenerateFingerprint(e)),
            }
        });

        if let Err(e) = res {
            warn!("Fingerprinting: {:?}", e);
        }
    }

    Ok(discoveries)
}

#[tracing::instrument]
fn list_container_layers(layer_path: &PathBuf) -> Result<HashSet<PathBuf>, Error> {
    let mut layers = HashSet::new();

    let mut container = unpack(layer_path)?;
    for entry in wrap_tar_err!("list entries", container.entries())? {
        let entry = match entry {
            Ok(entry) => entry,
            Err(e) => {
                warn!("Reading entry {:?}", e);
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
            .map_err(|e| Error::ParseManifest(layer_path.to_owned(), e))?;

        for manifest in manifests {
            layers.extend(manifest.layers);
        }

        // There's only one manifest file.
        break;
    }

    Ok(layers)
}

#[track_caller]
#[tracing::instrument(skip(reader))]
fn buffer(mut reader: impl Read) -> Result<Vec<u8>, Error> {
    let mut buf = Vec::new();
    reader.read_to_end(&mut buf).map_err(Error::ReadBuffer)?;
    Ok(buf)
}

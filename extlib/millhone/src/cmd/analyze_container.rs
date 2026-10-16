use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{BufReader, BufWriter, Read},
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

use super::go_buildinfo::{is_candidate_binary, scan_go_buildinfo, GoBuildInfo, GoModule};

#[derive(Debug, Parser, Getters)]
#[getset(get = "pub")]
#[clap(version)]
pub struct Subcommand {
    /// The tar file image to search and fingerprint jars in.
    image_tar_file: PathBuf,
}

const JAR_OBSERVATION: &str = "v1.discover.binary.jar";
const GO_BINARY_OBSERVATION: &str = "v1.discover.binary.go";

/// Only sniff regular files at least this large; Go binaries are never tiny.
/// (u64 because that's what tar header sizes are.)
const MIN_GO_BINARY_SIZE: u64 = 4096;

/// Magic bytes identifying a gzip stream.
const GZIP_MAGIC: [u8; 2] = [0x1f, 0x8b];

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

/// A Go binary discovered in a layer, with the module list parsed from its
/// embedded buildinfo.
#[derive(Debug, PartialEq, Eq, Serialize, Clone)]
struct DiscoveredGoBinary {
    kind: &'static str,
    path: PathBuf,
    go_version: String,
    main_module: Option<GoModule>,
    modules: Vec<GoModule>,
}

impl DiscoveredGoBinary {
    fn new(path: PathBuf, info: GoBuildInfo) -> Self {
        DiscoveredGoBinary {
            kind: GO_BINARY_OBSERVATION,
            path,
            go_version: info.go_version,
            main_module: info.main_module,
            modules: info.modules,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Serialize, TypedBuilder)]
struct ContainerAnalysis {
    /// Jars and fingerprints associated with each layer in a jar file.
    discovered_jars: HashMap<LayerPath, Vec<DiscoveredJar>>,

    /// Go binaries (with their embedded module lists) per layer.
    discovered_go_binaries: HashMap<LayerPath, Vec<DiscoveredGoBinary>>,
}

#[tracing::instrument]
pub fn main(opts: Subcommand) -> Result<()> {
    let tar_filename = opts.image_tar_file();
    let analysis = binaries_in_container(opts.image_tar_file())
        .with_context(|| format!("analyze container: {:?}", tar_filename))?;

    let mut stdout = BufWriter::new(std::io::stdout());
    serde_json::to_writer(&mut stdout, &analysis).context("Serialize Results")
}

/// Extracts the container (saved via `docker save`) and finds JAR files and Go
/// binaries inside any layer. JARs are fingerprinted; Go binaries have their
/// embedded buildinfo module list parsed. Everything is reported along with its
/// layer and path.
#[tracing::instrument]
fn binaries_in_container(image_path: &PathBuf) -> Result<ContainerAnalysis> {
    info!("inspecting container");
    let layers = list_container_layers(image_path)?;
    let mut jar_discoveries = HashMap::new();
    let mut go_discoveries = HashMap::new();

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
        let (layer_jars, layer_go_binaries) =
            scan_layer(entry).with_context(|| format!("read layer '{layer:?}'"))?;
        jar_discoveries.insert(LayerPath(layer.clone()), layer_jars);
        go_discoveries.insert(LayerPath(layer), layer_go_binaries);
    }

    Ok(ContainerAnalysis {
        discovered_jars: jar_discoveries,
        discovered_go_binaries: go_discoveries,
    })
}

/// Open and unpack a file and put it into a tar.
/// This is done repeatedly because `entries()` can only be read once from an Archive.
#[tracing::instrument]
fn unpack(path: &PathBuf) -> Result<Archive<File>> {
    let file = File::open(path).context("open tar file")?;
    Ok(tar::Archive::new(file))
}

/// Layer blobs are plain tars in the legacy `docker save` layout, but gzipped
/// tars in the OCI layout modern Docker emits. Peek at the magic bytes and
/// transparently decompress when needed.
#[tracing::instrument(skip(entry))]
fn scan_layer(
    entry: Entry<'_, impl Read>,
) -> Result<(Vec<DiscoveredJar>, Vec<DiscoveredGoBinary>)> {
    let mut reader = BufReader::new(entry);
    // A single `read` may legally return fewer bytes than requested even when
    // more remain, so loop until both magic bytes (or EOF) arrive, then
    // stitch them back onto the front of the stream.
    let mut magic = [0u8; GZIP_MAGIC.len()];
    let mut filled = 0;
    while filled < magic.len() {
        match reader.read(&mut magic[filled..]) {
            Ok(0) => break,
            Ok(n) => filled += n,
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
            Err(e) => return Err(e).context("detect layer compression"),
        }
    }
    let is_gzip = filled == magic.len() && magic == GZIP_MAGIC;
    let reader = std::io::Cursor::new(magic)
        .take(filled as u64)
        .chain(reader);
    if is_gzip {
        scan_layer_tar(flate2::read::GzDecoder::new(reader))
    } else {
        scan_layer_tar(reader)
    }
}

fn scan_layer_tar(reader: impl Read) -> Result<(Vec<DiscoveredJar>, Vec<DiscoveredGoBinary>)> {
    let mut discoveries = Vec::new();
    let mut go_discoveries = Vec::new();

    let mut entry_archive = tar::Archive::new(reader);
    for entry in entry_archive.entries().context("list entries in layer")? {
        let mut entry = entry.context("read entry")?;
        let path = entry.path().context("read path")?;
        if !path.to_string_lossy().ends_with(".jar") {
            let path = path.to_path_buf();
            match maybe_go_binary(&mut entry, &path) {
                Some(discovered) => go_discoveries.push(discovered),
                None => debug!(?path, "skipped: not a jar file or Go binary"),
            }
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

    Ok((discoveries, go_discoveries))
}

/// Sniff a non-jar layer entry for an embedded Go buildinfo section.
/// Anything that isn't a parseable Go binary returns `None` rather than
/// failing the scan, so one unreadable entry can't sink the layer.
fn maybe_go_binary(entry: &mut Entry<'_, impl Read>, path: &Path) -> Option<DiscoveredGoBinary> {
    if !entry.header().entry_type().is_file() {
        return None;
    }
    let size = entry.header().size().unwrap_or(0);
    if size < MIN_GO_BINARY_SIZE {
        return None;
    }

    // Sniffing consumes the entry's leading bytes (an `Entry` is a forward-only
    // reader), so stitch the prefix back onto the front of the stream: the
    // buildinfo scan needs offsets relative to the start of the file.
    let mut prefix = [0u8; 64];
    if let Err(e) = entry.read_exact(&mut prefix) {
        debug!(?path, "skipped: failed to read file prefix: {e:?}");
        return None;
    }
    if !is_candidate_binary(&prefix) {
        return None;
    }

    debug!(?path, "candidate binary; scanning for Go buildinfo");
    let reader = std::io::Cursor::new(prefix).chain(entry);
    scan_go_buildinfo(reader).map(|info| DiscoveredGoBinary::new(path.to_path_buf(), info))
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
  },
  "discovered_go_binaries": {
    "blobs/sha256/61aed1a8baa251dee118b9ab203c1e420f0eda0a9b3f9322d67d235dd27a12ee": [],
    "blobs/sha256/0e4613a3c620a37d93aca05039001fb5a6063c9d9cfb0935e3aa984025f31198": [],
    "blobs/sha256/632e84390ad558f9db0524f5e38a0af3e79c623a46bdce8a5e6a1761041b9850": [],
    "blobs/sha256/054f94aa7ce72b59cd6abac5462f77f0645b2f1a7b17e55d8f847a6da58c90db": []
  }
}
"#;

    #[test]
    fn it_finds_expected_output() {
        let image_tar_file =
            PathBuf::from("../../test/App/Fossa/Container/testdata/jar_test_container.tar");

        let res = binaries_in_container(&image_tar_file)
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
    fn it_analyzes_nested_jars_image() {
        let nested_jars_millhone_out: String = format!(
            r#"
        {{
          "discovered_jars": {{
            "88a896b18358cbccbf66cc1c3dcd0d2d61504c5bf41284c551e47f230675534f/layer.tar": [
              {{
                "kind": "v1.discover.binary.jar",
                "path": "jars/middle.jar",
                "fingerprints": {{
                  "v1.mavencentral.jar": "zpVxCUcFUE/F+WFYr7bUWEyA0Go=",
                  "v1.raw.jar": "b/9aNwWFlSqOwk+nZaG9zot8q+PENy/tn8Y0Xe/y3XY=",
                  "sha_256": "5Vh9z8oEKud8flIitzCE/rLMfbFlszhGkQbVxfHgg8U=",
                  "v1.class.jar": "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
                }}
              }},
              {{
                "kind": "v1.discover.binary.jar",
                "path": "jars/middle.jar{separator}deepest.jar",
                "fingerprints": {{
                  "sha_256": "vpyhj/ImCwcAx4HZTP23XV9yTdNdRF5NQFj8eV5NOHM=",
                  "v1.class.jar": "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=",
                  "v1.mavencentral.jar": "yPAF7stJN1sIjsM4/hSWcGilyz8=",
                  "v1.raw.jar": "UMQ1yS7xM6tF4YMvAWz8UP6+qAIRq3JauBoiTlVUNkM="
                }}
              }}
            ],
            "2cd0dec90e9f3f920397ed6bf0ba740493620a99bb20b79d2c4c8159948439e4/layer.tar": [],
            "5a99cb0cd20c916ca7444b625ff06e3afe6a1b4349c44c3ba11eb054daf5fda4/layer.tar": [],
            "9fea496e8349f2c33fe177df27e4369f08cff62ad40168183493d9de3d6832e5/layer.tar": [
              {{
                "kind": "v1.discover.binary.jar",
                "path": "jars/deepest.jar",
                "fingerprints": {{
                  "v1.class.jar": "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=",
                  "sha_256": "vpyhj/ImCwcAx4HZTP23XV9yTdNdRF5NQFj8eV5NOHM=",
                  "v1.mavencentral.jar": "yPAF7stJN1sIjsM4/hSWcGilyz8=",
                  "v1.raw.jar": "UMQ1yS7xM6tF4YMvAWz8UP6+qAIRq3JauBoiTlVUNkM="
                }}
              }}
            ],
            "c65b9197c11847b3f36c822b9c57f417af6f721ae6719f8eb3bd334c3516796e/layer.tar": [],
            "792ccfdf114be140500ea1d6b99ae7ff0cae6a19b247f886328d4b8a01b8869c/layer.tar": [
              {{
                "kind": "v1.discover.binary.jar",
                "path": "sym-link-test/sym.jar",
                "fingerprints": {{
                  "comment_stripped:sha_256": "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=",
                  "sha_256": "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=",
                  "v1.mavencentral.jar": "2jmj7l5rSw0yVb/vlWAYkK/YBwk="
                }}
              }}
            ],
            "d8abb0d1e8708fb1b5b79bcdd098898becfe22019d6b70781974743a90415724/layer.tar": [
              {{
                "kind": "v1.discover.binary.jar",
                "path": "jars/top.jar",
                "fingerprints": {{
                  "v1.raw.jar": "Qvy1Y7ZCnHpGfy12XszUCq8zyzsTeZ2f0HiMkZKRkzY=",
                  "sha_256": "RXo50J5YsgsHl56Vfcc3Ee9epeJ2XXmO56Zi/4DAQZM=",
                  "v1.mavencentral.jar": "NWCmsGqwY5JV85qtij8Nf/IQidw=",
                  "v1.class.jar": "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
                }}
              }},
              {{
                "kind": "v1.discover.binary.jar",
                "path": "jars/top.jar{separator}middle.jar",
                "fingerprints": {{
                  "sha_256": "5Vh9z8oEKud8flIitzCE/rLMfbFlszhGkQbVxfHgg8U=",
                  "v1.class.jar": "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=",
                  "v1.mavencentral.jar": "zpVxCUcFUE/F+WFYr7bUWEyA0Go=",
                  "v1.raw.jar": "b/9aNwWFlSqOwk+nZaG9zot8q+PENy/tn8Y0Xe/y3XY="
                }}
              }},
              {{
                "kind": "v1.discover.binary.jar",
                "path": "jars/top.jar{separator}middle.jar{separator}deepest.jar",
                "fingerprints": {{
                  "v1.mavencentral.jar": "yPAF7stJN1sIjsM4/hSWcGilyz8=",
                  "sha_256": "vpyhj/ImCwcAx4HZTP23XV9yTdNdRF5NQFj8eV5NOHM=",
                  "v1.raw.jar": "UMQ1yS7xM6tF4YMvAWz8UP6+qAIRq3JauBoiTlVUNkM=",
                  "v1.class.jar": "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
                }}
              }}
            ]
          }},
          "discovered_go_binaries": {{
            "88a896b18358cbccbf66cc1c3dcd0d2d61504c5bf41284c551e47f230675534f/layer.tar": [],
            "2cd0dec90e9f3f920397ed6bf0ba740493620a99bb20b79d2c4c8159948439e4/layer.tar": [],
            "5a99cb0cd20c916ca7444b625ff06e3afe6a1b4349c44c3ba11eb054daf5fda4/layer.tar": [],
            "9fea496e8349f2c33fe177df27e4369f08cff62ad40168183493d9de3d6832e5/layer.tar": [],
            "c65b9197c11847b3f36c822b9c57f417af6f721ae6719f8eb3bd334c3516796e/layer.tar": [],
            "792ccfdf114be140500ea1d6b99ae7ff0cae6a19b247f886328d4b8a01b8869c/layer.tar": [],
            "d8abb0d1e8708fb1b5b79bcdd098898becfe22019d6b70781974743a90415724/layer.tar": []
          }}
        }}
        "#,
            separator = std::path::MAIN_SEPARATOR_STR.replace("\\", "\\\\")
        );
        let image_tar_file =
            PathBuf::from("../../test/App/Fossa/Container/testdata/nested_jars.tar");
        let res = binaries_in_container(&image_tar_file)
            .expect("Read jars out of container image.")
            .pipe(serde_json::to_value)
            .expect("encode as json");
        let expected: Value =
            serde_json::from_str(&nested_jars_millhone_out).expect("Parse expected json");
        pretty_assertions::assert_eq!(expected, res);
    }

    const BUILDINFO_FIXTURE: &[u8] = include_bytes!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/testdata/go-buildinfo/gh-2.86.0-darwin-arm64.bin"
    ));

    /// A fake ELF: candidate magic up front, the real buildinfo fixture at a
    /// 16-byte-aligned offset, padded past MIN_GO_BINARY_SIZE.
    fn fake_go_binary() -> Vec<u8> {
        let mut buf = vec![0u8; 64];
        buf[..4].copy_from_slice(b"\x7fELF");
        buf.extend_from_slice(BUILDINFO_FIXTURE);
        if buf.len() < MIN_GO_BINARY_SIZE as usize {
            buf.resize(MIN_GO_BINARY_SIZE as usize, 0);
        }
        buf
    }

    fn tar_with(files: &[(&str, &[u8])]) -> Vec<u8> {
        let mut builder = tar::Builder::new(Vec::new());
        for (path, contents) in files {
            let mut header = tar::Header::new_gnu();
            header.set_size(contents.len() as u64);
            header.set_mode(0o644);
            header.set_cksum();
            builder
                .append_data(&mut header, path, *contents)
                .expect("append tar entry");
        }
        builder.into_inner().expect("finish tar")
    }

    fn gzip(data: &[u8]) -> Vec<u8> {
        use std::io::Write;
        let mut encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
        encoder.write_all(data).expect("gzip write");
        encoder.finish().expect("gzip finish")
    }

    /// Wrap a layer blob in an outer image tar and run scan_layer over it.
    fn scan(layer: &[u8]) -> Result<(Vec<DiscoveredJar>, Vec<DiscoveredGoBinary>)> {
        let outer = tar_with(&[("layer.tar", layer)]);
        let mut archive = tar::Archive::new(std::io::Cursor::new(outer));
        let entry = archive
            .entries()
            .expect("iterate outer tar")
            .next()
            .expect("outer tar has one entry")
            .expect("read outer entry");
        scan_layer(entry)
    }

    /// Run maybe_go_binary against the first entry of a tar built from `files`.
    fn sniff_first(files: &[(&str, &[u8])]) -> Option<DiscoveredGoBinary> {
        let data = tar_with(files);
        let mut archive = tar::Archive::new(std::io::Cursor::new(data));
        let mut entry = archive
            .entries()
            .expect("iterate tar")
            .next()
            .expect("tar has one entry")
            .expect("read entry");
        let path = entry.path().expect("entry path").to_path_buf();
        maybe_go_binary(&mut entry, &path)
    }

    #[test]
    fn scans_plain_and_gzipped_layers_identically() {
        let binary = fake_go_binary();
        let layer = tar_with(&[("usr/bin/app", &binary)]);
        let (_, plain) = scan(&layer).expect("scan plain layer");
        let (_, gzipped) = scan(&gzip(&layer)).expect("scan gzipped layer");
        assert_eq!(plain.len(), 1);
        assert_eq!(plain[0].path, PathBuf::from("usr/bin/app"));
        assert_eq!(plain, gzipped);
    }

    #[test]
    fn scans_empty_layer_without_error() {
        let empty_tar = tar_with(&[]);
        let (jars, go_binaries) = scan(&empty_tar).expect("scan empty tar layer");
        assert!(jars.is_empty());
        assert!(go_binaries.is_empty());

        let (jars, go_binaries) = scan(&[]).expect("scan zero-byte layer");
        assert!(jars.is_empty());
        assert!(go_binaries.is_empty());
    }

    #[test]
    fn skips_undersized_candidates() {
        assert!(sniff_first(&[("bin", b"\x7fELF but far too small")]).is_none());
    }

    #[test]
    fn skips_non_file_entries() {
        let mut builder = tar::Builder::new(Vec::new());
        let mut header = tar::Header::new_gnu();
        header.set_entry_type(tar::EntryType::Directory);
        header.set_size(0);
        header.set_mode(0o755);
        header.set_cksum();
        builder
            .append_data(&mut header, "some-dir/", &[][..])
            .expect("append dir entry");
        let data = builder.into_inner().expect("finish tar");
        let mut archive = tar::Archive::new(std::io::Cursor::new(data));
        let mut entry = archive
            .entries()
            .expect("iterate tar")
            .next()
            .expect("tar has one entry")
            .expect("read entry");
        assert!(maybe_go_binary(&mut entry, Path::new("some-dir/")).is_none());
    }

    #[test]
    fn skips_non_binary_files() {
        let text = vec![b'a'; MIN_GO_BINARY_SIZE as usize];
        assert!(sniff_first(&[("notes.txt", &text)]).is_none());
    }

    #[test]
    fn skips_binaries_without_buildinfo() {
        let mut elf = vec![0u8; MIN_GO_BINARY_SIZE as usize];
        elf[..4].copy_from_slice(b"\x7fELF");
        assert!(sniff_first(&[("bin", &elf)]).is_none());
    }

    #[test]
    fn finds_buildinfo_deep_in_a_large_binary() {
        // Buildinfo placed past the streaming scan window, so the rolling
        // search (not just the sniffed prefix) has to find it.
        let mut buf = vec![0u8; 256 * 1024];
        buf[..4].copy_from_slice(b"\x7fELF");
        buf.extend_from_slice(BUILDINFO_FIXTURE);
        let discovered = sniff_first(&[("usr/bin/big", &buf)]).expect("should discover");
        assert_eq!(discovered.go_version, "go1.25.6");
        assert_eq!(discovered.modules.len(), 161);
    }
}

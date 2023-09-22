//! Types and functionality for extracting snippets from code units (generally files on disk).
//!
//! [`Snippet`] is the primary entrypoint to this file, especially `Snippet::extract`.

use std::{
    collections::HashSet,
    hash::Hash,
    path::{Path, PathBuf},
};

use base64::prelude::*;
use clap::{Parser, ValueEnum};
use derive_more::From;
use getset::{CopyGetters, Getters};
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use snippets::{language::c99_tc3, Extractor};
use strum::{Display, EnumIter, IntoEnumIterator};
use tap::Pipe;
use thiserror::Error;
use tracing::debug;
use typed_builder::TypedBuilder;

/// Errors encountered in this module.
#[derive(Debug, Error)]
pub enum Error {
    /// Encountered when reading a file.
    // Use `#[source]` instead of `#[from]` because `std::io::Error` is generic.
    // For convenience, you can `.map_err(Error::ReadFile)`.
    #[error("read file")]
    ReadFile(#[source] std::io::Error),

    /// Encountered when extracting snippets from a file.
    #[error("extract snippets")]
    ExtractSnippets(#[from] snippets::Error),
}

/// Options for snippet extraction.
#[derive(Debug, Parser, Getters)]
#[getset(get = "pub")]
#[clap(version)]
pub struct Options {
    /// Extract this combination of targets.
    /// Specify the argument multiple times to indicate additional options.
    #[clap(long = "target", default_value = "function")]
    targets: Vec<Target>,

    /// Extract this combination of kinds.
    /// Specify the argument multiple times to indicate additional options.
    #[clap(long = "kind", default_value = "full")]
    kinds: Vec<Kind>,

    /// Use this combination of transforms.
    /// Specify the argument multiple times to indicate additional options.
    #[clap(long = "transform")]
    transforms: Vec<Transform>,

    /// Target the provided directory for snippet ingestion.
    target: PathBuf,
}

impl From<Options> for snippets::Options {
    fn from(value: Options) -> Self {
        Self::from(&value)
    }
}

impl From<&Options> for snippets::Options {
    fn from(value: &Options) -> Self {
        snippets::Options::new(
            value.targets().iter().copied().map(Into::into),
            value.kinds().iter().copied().map(Into::into),
            value.transforms().iter().copied().map(Into::into),
        )
    }
}

/// The targets of snippets to extract.
#[derive(Debug, Clone, Copy, ValueEnum, Display)]
#[strum(serialize_all = "snake_case")]
pub enum Target {
    /// Targets function defintions as snippets.
    Function,
}

impl From<Target> for snippets::Target {
    fn from(value: Target) -> Self {
        match value {
            Target::Function => snippets::Target::Function,
        }
    }
}

/// The kind of item this snippet represents.
#[derive(Debug, Clone, Copy, ValueEnum, Display)]
#[strum(serialize_all = "snake_case")]
pub enum Kind {
    /// The signature of the snippet.
    Signature,

    /// The body of the snippet.
    Body,

    /// Both signature and body in one snippet.
    Full,
}

impl From<Kind> for snippets::Kind {
    fn from(value: Kind) -> Self {
        match value {
            Kind::Signature => snippets::Kind::Signature,
            Kind::Body => snippets::Kind::Body,
            Kind::Full => snippets::Kind::Full,
        }
    }
}

/// The normalization used to extract this snippet.
#[derive(Debug, Clone, Copy, ValueEnum, Display)]
#[strum(serialize_all = "snake_case")]
pub enum Transform {
    /// Transform the text to have any comments removed and whitespace normalized.
    Code,

    /// Generated with any comments removed.
    Comment,

    /// Generated with any whitespace characters (including newlines) normalized to a single space.
    Space,
}

impl From<Transform> for snippets::Transform {
    fn from(value: Transform) -> Self {
        match value {
            Transform::Code => snippets::Transform::Code,
            Transform::Comment => snippets::Transform::Comment,
            Transform::Space => snippets::Transform::Space,
        }
    }
}

/// A snippet is a specific unit of code that the service wants to match later.
///
/// A snippet fingerprint is a deterministic representation of the code
/// present in the project at the provided byte range (indicated by `byte_start` and `byte_end`)
/// in the provided `file_path` (relative to the root of the package)
/// as rendered with the provided `target`, `kind`, and `method`.
///
/// The definitions and possible values for most of these properties,
/// especially `target`, `kind`, `method`, and `language`, are in the snippet library.
/// They don't intrinsically mean anything to the Millhone server or databases.
///
/// Multiple snippets may validly exist for any combination of
/// `(fingerprint, locator, target, kind, method, file_path, start_byte, end_byte)`,
/// so these properties together constitute the "unique key" of a collection of snippets.
///
/// The rest of the data on the type is meant for more human-friendly display of information
/// (for example line and column ranges intead of byte ranges)
/// along with diagnostics or additional filtering criteria
/// (for example the detected language and ingestion id).
///
/// For more information on how snippets are generated, see: https://github.com/fossas/foundation-libs/tree/master/snippets
#[derive(
    Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, CopyGetters, Getters, TypedBuilder,
)]
pub struct Snippet {
    /// A snippet fingerprint is a deterministic representation of the code
    /// present in the project at the provided byte range (indicated by `byte_start` and `byte_end`)
    /// in the provided `file_path` (relative to the root of the package)
    /// as rendered with the provided `target`, `kind`, and `method`.
    #[getset(get = "pub")]
    fingerprint: Fingerprint,

    /// The target in the source code that was found and used to extract this snippet.
    #[getset(get = "pub")]
    target: String,

    /// The kind of snippet this represents.
    #[getset(get = "pub")]
    kind: String,

    /// The method used to normalize the snippet.
    #[getset(get = "pub")]
    method: String,

    /// The file path, relative to a project's root, which contained this snippet.
    #[getset(get = "pub")]
    file_path: String,

    /// The byte index at which the snippet starts in the code file.
    #[getset(get_copy = "pub")]
    byte_start: i64,

    /// The byte index at which the snippet ends in the code file.
    #[getset(get_copy = "pub")]
    byte_end: i64,

    /// The line index at which the snippet starts in the code file.
    #[getset(get_copy = "pub")]
    line_start: i32,

    /// The line index at which the snippet ends in the code file.
    #[getset(get_copy = "pub")]
    line_end: i32,

    /// The col index at which the snippet starts in the code file.
    #[getset(get_copy = "pub")]
    col_start: i32,

    /// The col index at which the snippet ends in the code file.
    #[getset(get_copy = "pub")]
    col_end: i32,

    /// The detected code language for the snippet.
    #[getset(get = "pub")]
    language: String,
}

impl Snippet {
    /// Create a new instance from an extracted snippet.
    pub fn from<L: snippets::Language>(
        scan_root: &Path,
        path: &Path,
        content: &[u8],
        snippet: snippets::Snippet<L>,
    ) -> Self {
        let location = Location::new(snippet.metadata().location(), content);
        let display_path = path.strip_prefix(scan_root).unwrap_or(path);
        Self::builder()
            .fingerprint(
                snippet
                    .fingerprint()
                    .as_bytes()
                    .to_owned()
                    .pipe(Fingerprint),
            )
            .target(snippets::Target::Function.to_string())
            .kind(snippet.metadata().kind().to_string())
            .method(snippet.metadata().method().to_string())
            .file_path(display_path.to_string_lossy().to_string())
            .byte_start(location.byte_start() as _)
            .byte_end(location.byte_end() as _)
            .line_start(location.line_start as _)
            .line_end(location.line_end as _)
            .col_start(location.col_start as _)
            .col_end(location.col_end as _)
            .language(L::display().to_string())
            .build()
    }

    /// Extract instances from a file on disk.
    #[tracing::instrument]
    pub fn from_file(
        scan_root: &Path,
        opts: &snippets::Options,
        path: &Path,
    ) -> Result<HashSet<Self>, Error> {
        Self::from_file_with_content(scan_root, opts, path).map(|(found, _)| found)
    }

    /// Extract instances from a file on disk,
    /// returning the content of the file along with the snippet.
    ///
    /// If the file is not supported, the returned file content is empty.
    pub fn from_file_with_content(
        scan_root: &Path,
        opts: &snippets::Options,
        path: &Path,
    ) -> Result<(HashSet<Self>, Vec<u8>), Error> {
        match Support::by_ext(path) {
            Support::Unknown => {
                debug!("skipping: unknown support status for file");
                Ok((Default::default(), Default::default()))
            }
            Support::Unsupported => {
                debug!("skipping: file extension not supported");
                Ok((Default::default(), Default::default()))
            }
            Support::Supported(language) => {
                debug!("extracting snippets of language: {language}");
                let content = std::fs::read(path).map_err(Error::ReadFile)?;
                match language {
                    Language::C => c99_tc3::Extractor::extract(opts, &content)?
                        .pipe(collapse_raw)
                        .map(|snippet| Self::from(scan_root, path, &content, snippet))
                        .collect::<HashSet<_>>()
                        .pipe(|found| (found, content))
                        .pipe(Ok),
                }
            }
        }
    }

    /// Get the [`Location`] referenced by the snippet.
    pub fn location(&self) -> Location {
        Location::builder()
            .byte_start(self.byte_start as _)
            .byte_end(self.byte_end as _)
            .line_start(self.line_start as _)
            .line_end(self.line_end as _)
            .col_start(self.col_start as _)
            .col_end(self.col_end as _)
            .build()
    }
}

/// Equivalent to [`Snippet`], with the content copied from the input file.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Getters, TypedBuilder)]
#[getset(get = "pub")]
pub struct ContentSnippet {
    /// The snippet that was found.
    #[serde(flatten)]
    snippet: Snippet,

    /// The content of the file from which the snippet was extracted
    /// at the location indicated by the snippet.
    content: Vec<u8>,
}

impl ContentSnippet {
    /// Extract instances from a file on disk.
    #[tracing::instrument]
    pub fn from_file(
        scan_root: &Path,
        opts: &snippets::Options,
        path: &Path,
    ) -> Result<HashSet<Self>, Error> {
        let (found, content) = Snippet::from_file_with_content(scan_root, opts, path)?;
        found
            .into_iter()
            .map(|snippet| {
                let location = snippet.location();
                Self {
                    snippet,
                    content: location.extract_from(&content).to_owned(),
                }
            })
            .collect::<HashSet<_>>()
            .pipe(Ok)
    }
}

/// A deterministic representation of source code.
#[derive(Clone, PartialEq, Eq, Hash, From)]
pub struct Fingerprint(Vec<u8>);

impl Fingerprint {
    /// Read the fingerprint as a byte vec.
    pub fn to_vec(self) -> Vec<u8> {
        self.0
    }

    /// Read the fingerprint as a URL-safe Base64 encoded string.
    pub fn as_base64_url(&self) -> String {
        BASE64_URL_SAFE.encode(&self.0)
    }
}

impl std::fmt::Display for Fingerprint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&BASE64_STANDARD.encode(&self.0))
    }
}

impl std::fmt::Debug for Fingerprint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            f.debug_tuple("Fingerprint")
                .field(&self.to_string())
                .finish()
        } else {
            f.debug_tuple("Fingerprint").field(&self.0).finish()
        }
    }
}

impl serde::Serialize for Fingerprint {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let encoded = BASE64_STANDARD.encode(&self.0);
        serializer.serialize_str(&encoded)
    }
}

impl<'de> serde::Deserialize<'de> for Fingerprint {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let raw = String::deserialize(deserializer)?;
        let decoded = BASE64_STANDARD
            .decode(raw)
            .map_err(serde::de::Error::custom)?;
        Self(decoded).pipe(Ok)
    }
}

/// The location, using textual "line, column" indicators, for a snippet.
///
/// Byte locations are 0-based, while line and column indexes are 1-based:
/// ```ignore
/// // Remember that while `\n` is two columns as typed, it's 1 in actual text.
/// let input = b"hello\nworld\nand beyond!";
/// //            ^   ^  ^   ^  ^        ^
/// //   bytes:   0   4  6   10 12       21
/// // columns:   1   5  1   5  1        11
/// //   lines:   1      2      3
///```
///
/// Line end is inclusive: if a snippet starts on line 1 and ends before
/// the end of the line, `line_end` is line 1 as well.
///
/// Column end is inclusive: when displaying the snippet, the column
/// _indicated by_ this number should be included.
///
/// For example, if the full text was "hello world!",
/// and the snippet starts on byte 6 and ends on byte 10,
/// the returned `Location` looks like:
/// ```ignore
/// Location {
///   byte_start: 6,
///   byte_end: 10,
///   line_start: 1,
///   line_end: 1,
///   col_start: 7,
///   col_end: 11,
/// }
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, CopyGetters, TypedBuilder)]
#[getset(get_copy = "pub")]
pub struct Location {
    /// The byte index at which the snippet starts.
    byte_start: usize,

    /// The byte index at which the snippet ends.
    byte_end: usize,

    /// The line number on which the snippet starts.
    line_start: usize,

    /// The line number on which the snippet ends, inclusive.
    line_end: usize,

    /// The column at which the snippet starts on the line specified by `line_start`.
    col_start: usize,

    /// The column at which the snippet ends on the line specified by `line_end`, inclusive.
    col_end: usize,
}

impl Location {
    /// Derive an instance from a [`snippets::Location`] and the content to which it points.
    fn new(loc: snippets::Location, content: &[u8]) -> Self {
        let byte_start = loc.start_byte();
        let byte_end = loc.end_byte();
        let mut line_start = 1;
        let mut col_start = 1;

        const NEWLINE: u8 = b'\n';
        for b in content[..byte_start].iter().copied() {
            if b == NEWLINE {
                line_start += 1;
                col_start = 1;
            } else {
                col_start += 1;
            }
        }

        let mut line_end = line_start;
        let mut col_end = col_start;
        for b in content[byte_start..=byte_end].iter().copied() {
            if b == NEWLINE {
                line_end += 1;
                col_end = 1;
            } else {
                col_end += 1;
            }
        }

        Self {
            byte_start,
            // The end byte of `snippets::Location` is inclusive.
            byte_end: byte_end + 1,
            line_start,
            line_end,
            col_start,
            col_end,
        }
    }

    /// Extract the bytes indicated by a [`Location`] from a buffer.
    pub fn extract_from<'a>(&self, buf: &'a [u8]) -> &'a [u8] {
        &buf[self.byte_start..self.byte_end]
    }
}

/// The support status of a given unit of code (typically a file).
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display, Default)]
#[strum(serialize_all = "snake_case")]
pub enum Support {
    /// The support status for the language is unknown.
    #[default]
    Unknown,

    /// The language is known to be unsupported.
    Unsupported,

    /// The language is supported.
    Supported(Language),
}

impl Support {
    /// Determine whether a file is supported, and its language, based on its extension.
    pub fn by_ext(path: &Path) -> Self {
        path.extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| {
                Language::iter()
                    .find(|language| language.supports_file_extension(ext))
                    .map(Support::Supported)
                    .unwrap_or(Support::Unsupported)
            })
            .unwrap_or_default()
    }
}

/// Languages detected in source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display, EnumIter)]
#[strum(serialize_all = "snake_case")]
pub enum Language {
    /// The C programming language.
    C,
}

impl Language {
    /// Report supported file extensions for each variant.
    ///
    /// Note: supported extensions must not overlap.
    /// Languages with overlapping file extensions (e.g. C and C++ both use ".h" for header files)
    /// must instead specify a new language for just that file type.
    pub fn supported_file_extensions(self) -> &'static [&'static str] {
        match self {
            Language::C => &["c"],
        }
    }

    /// Report whether the language declares it supports files with the given file extension.
    pub fn supports_file_extension(self, ext: &str) -> bool {
        self.supported_file_extensions().contains(&ext)
    }
}

/// Collapse a set of extracted snippets such that snippets that are identical
/// other than along the axis of [`snippets::Kind`] and [`snippets::Method`] are collapsed into
/// the highest specificity form.
///
/// When scanning a project for a set of snippets, one code item (for example, a function)
/// will potentially generate many snippets of varying [`snippets::Kind`]s and [`snippets::Method`]s.
///
/// However, it's also possible for a given code item to be the same between different combinations;
/// consider the following sample:
/// ```not_rust
/// int main() {
///   printf("hello world\n");
///   return 0;
/// }
/// ```
///
/// This same results in the same output for all [`snippets::Kind`]s for [`snippets::Method::Raw`]
/// and [`snippets::Method::Normalized`] with the [`snippets::Transform::Comment`] transform.
/// This introduces noise both in the database and for the user when matches are performed!
///
/// Fortunately, [`snippets::Method`] implements [`Ord`], such that "higher specificity" snippets
/// are ordered higher when compared with [`Ord`]. This means that to choose the most correct
/// single representative, we can bucket equivalent snippets together and then pick the one
/// that is highest sorted. Specificity is sorted by [`snippets::Kind`], then [`snippets::Method`].
///
/// Snippets are considered equivalent when the [`snippets::Snippet::fingerprint`] field matches
/// and the [`snippets::Metadata::location`] field inside [`snippets::Snippet::metadata`] matches.
#[tracing::instrument(skip_all, fields(input_count, collapsed_count))]
fn collapse_raw<L>(
    snippets: impl IntoIterator<Item = snippets::Snippet<L>>,
) -> impl Iterator<Item = snippets::Snippet<L>> {
    let mut input_count = 0usize;
    let mut grouped = snippets
        .into_iter()
        .inspect(|_| input_count += 1)
        .into_group_map_by(|snippet| {
            (snippet.fingerprint().clone(), snippet.metadata().location())
        });

    for (_, group) in grouped.iter_mut() {
        group.sort_by_key(|s| (s.metadata().kind(), s.metadata().method()));
    }

    // Returning an iter so can't count directly,
    // but it's known that at most one value per key is returned.
    tracing::Span::current().record("input_count", input_count);
    tracing::Span::current().record("collapsed_count", grouped.len());

    // Pop from the end, since `sort_by_key` sorts in ascending order
    // (so higher specificity snippets are sorted later in the vec).
    grouped.into_values().filter_map(|mut group| group.pop())
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use maplit::hashmap;
    use pretty_assertions::assert_eq;
    use rand::{seq::SliceRandom, thread_rng, Rng};

    use super::*;

    #[test]
    fn language_supported_extensions_do_not_overlap() {
        let languages = Language::iter()
            .flat_map(|language| {
                language
                    .supported_file_extensions()
                    .iter()
                    .map(|ext| (*ext, language))
                    .collect_vec()
            })
            .into_group_map();

        let overlaps = languages
            .into_iter()
            .filter(|(_, v)| v.len() > 1)
            .collect::<HashMap<_, _>>();
        assert_eq!(overlaps, hashmap! {});
    }

    #[test]
    fn reports_supported_ext() {
        let exts = Language::iter().flat_map(|lang| {
            lang.supported_file_extensions()
                .iter()
                .map(move |ext| (lang, *ext))
        });

        for (lang, ext) in exts {
            let file_name = PathBuf::from(format!("some_path.{ext}"));
            let supported = Support::by_ext(&file_name);
            assert_eq!(supported, Support::Supported(lang));
        }
    }

    #[test]
    fn unknown_on_missing_ext() {
        let file_name = PathBuf::from("some_path_no_ext");
        let supported = Support::by_ext(&file_name);
        assert_eq!(supported, Support::Unknown);
    }

    #[test]
    fn unsupported_on_invalid_ext() {
        let file_name = PathBuf::from("some_path.some_ext");
        let supported = Support::by_ext(&file_name);
        assert_eq!(supported, Support::Unsupported);
    }

    #[test]
    fn text_location_simple() {
        let input = b"hello world";
        //            ^     ^   ^
        // columns:   1     7   11
        let location = snippets::Location::from(6..10);
        let expected = Location::builder()
            .byte_start(6)
            .byte_end(10)
            .line_start(1)
            .line_end(1)
            .col_start(7)
            .col_end(11)
            .build();
        let got = Location::new(location, input);
        assert_eq!(got, expected);
    }

    #[test]
    fn text_location_complicated() {
        // Remember that while `\n` is two columns as typed, it's 1 in actual text.
        let input = b"hello\nworld\nand beyond!";
        //            ^   ^  ^   ^  ^        ^
        // columns:   1   5  1   5  1        11
        //   lines:   1      2      3
        let location = snippets::Location::from(6..22);
        let expected = Location::builder()
            .byte_start(6)
            .byte_end(22)
            .line_start(2)
            .line_end(3)
            .col_start(1)
            .col_end(11)
            .build();
        let got = Location::new(location, input);
        assert_eq!(got, expected);
    }

    #[test]
    fn location_extract_from() {
        let example = "#include <stdio.h>  int main() {}";
        let location = Location::builder()
            .byte_start(20)
            .byte_end(30)
            .line_start(1)
            .line_end(1)
            .col_start(21)
            .col_end(31)
            .build();

        let got = location.extract_from(example.as_bytes());
        assert_eq!(got, b"int main()");
    }

    #[test]
    fn snippets_location_extract_from() {
        let example = "#include <stdio.h>  int main() {}";
        let location = snippets::Location::from(20..30);
        let location = Location::new(location, example.as_bytes());

        let got = location.extract_from(example.as_bytes());
        assert_eq!(got, b"int main()");
    }

    #[test]
    fn collapse_snippets_works() {
        let buffer = |bytes: &[u8]| snippets::text::Buffer::new(bytes);
        let kinds = snippets::Kind::iter().collect_vec();
        let methods = snippets::Transform::iter()
            .map(snippets::Method::Normalized)
            .chain(std::iter::once(snippets::Method::Raw))
            .collect_vec();

        let rng_buf = || {
            thread_rng()
                .gen::<[u8; 32]>()
                .pipe(snippets::text::Buffer::new)
        };
        let rng_loc = || -> snippets::Location {
            (thread_rng().gen_range(1000..10000)..thread_rng().gen_range(10000..100000)).into()
        };
        let rng_kind = || {
            kinds
                .choose(&mut thread_rng())
                .copied()
                .expect("choose kind")
        };
        let rng_method = || {
            methods
                .choose(&mut thread_rng())
                .copied()
                .expect("choose method")
        };

        let known_content = buffer(b"some content");
        let known_fp = buffer(b"some fingerprint");
        let known_loc = snippets::Location::from(10..20);

        // These snippets should all be collapsed. All possible kinds and methods are represented here,
        // with a fully equivalent fingerprint and location.
        // Content isn't considered by the function, but make it known too for completeness.
        let homogenous = kinds
            .clone()
            .into_iter()
            .cartesian_product(methods.clone())
            .map(|(kind, method)| -> snippets::Snippet<c99_tc3::Language> {
                snippets::Snippet::builder()
                    .content(known_content.clone())
                    .fingerprint(known_fp.clone())
                    .metadata(snippets::Metadata::new(kind, method, known_loc))
                    .build()
            });

        // These snippets should match either the fingerprint or the location
        // of the homogenous snippets, but not both.
        // The uniqueness call is here to ensure the interleaved sample below gets a good range.
        let almost_homogenous =
            std::iter::repeat_with(|| -> snippets::Snippet<c99_tc3::Language> {
                let matching_fp = thread_rng().gen_bool(0.5);
                snippets::Snippet::builder()
                    .content(known_content.clone())
                    .fingerprint(if matching_fp {
                        known_fp.clone()
                    } else {
                        rng_buf()
                    })
                    .metadata(snippets::Metadata::new(
                        rng_kind(),
                        rng_method(),
                        if matching_fp { rng_loc() } else { known_loc },
                    ))
                    .build()
            })
            .unique_by(|snippet| (snippet.fingerprint().clone(), snippet.metadata().location()));

        // These snippets shouldn't match the homogenous snippets in any way.
        // The uniqueness call is here to ensure the interleaved sample below gets a good range.
        let heterogenous = std::iter::repeat_with(|| -> snippets::Snippet<c99_tc3::Language> {
            snippets::Snippet::builder()
                .content(rng_buf())
                .fingerprint(rng_buf())
                .metadata(snippets::Metadata::new(rng_kind(), rng_method(), rng_loc()))
                .build()
        })
        .unique_by(|snippet| (snippet.fingerprint().clone(), snippet.metadata().location()));

        let combined = homogenous
            .interleave(heterogenous)
            .interleave(almost_homogenous)
            .take(100)
            .collect_vec();

        // All the random data is really just there to smoke test the function.
        // At the end of the day, we just want to see that the homogenous snippets were collapsed down to
        // a single result (and that it was the highest precedence one),
        // and that the almost homogenous snippets didn't subtly mess this up.
        let collapsed = collapse_raw(combined)
            .filter(|snippet| {
                snippet.fingerprint() == &known_fp && snippet.metadata().location() == known_loc
            })
            .collect_vec();

        // Leave it up to `snippets` to tell us which of the kinds and methods are highest precedence.
        let highest_precedence_homogenous = snippets::Snippet::builder()
            .content(known_content)
            .fingerprint(known_fp)
            .metadata(snippets::Metadata::new(
                kinds
                    .iter()
                    .sorted_unstable()
                    .last()
                    .copied()
                    .expect("get highest precedence kind"),
                methods
                    .iter()
                    .sorted_unstable()
                    .last()
                    .copied()
                    .expect("get highest precedence method"),
                known_loc,
            ))
            .build();

        assert_eq!(collapsed, vec![highest_precedence_homogenous]);
    }
}

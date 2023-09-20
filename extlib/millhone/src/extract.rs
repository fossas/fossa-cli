//! Types and functionality for extracting snippets from code units (generally files on disk).
//!
//! [`Snippet`] is the primary entrypoint to this file, especially `Snippet::extract`.

use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};

use base64::prelude::*;
use clap::{Parser, ValueEnum};
use derive_more::From;
use getset::{CopyGetters, Getters};
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
        path: &Path,
        content: &[u8],
        snippet: snippets::Snippet<L>,
    ) -> Self {
        let location = snippet.metadata().location();
        let text_loc = TextLocation::new(&location, content);
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
            .file_path(path.to_string_lossy().to_string())
            .byte_start(location.start_byte() as _)
            .byte_end(location.end_byte() as _)
            .line_start(text_loc.line_start as _)
            .line_end(text_loc.line_end as _)
            .col_start(text_loc.col_start as _)
            .col_end(text_loc.col_end as _)
            .language(L::display().to_string())
            .build()
    }

    /// Extract instances from a file on disk.
    #[tracing::instrument]
    pub fn from_file(opts: &snippets::Options, path: &Path) -> Result<HashSet<Self>, Error> {
        match Support::by_ext(path) {
            Support::Unknown => {
                debug!("skipping: unknown support status for file");
                Ok(Default::default())
            }
            Support::Unsupported => {
                debug!("skipping: file extension not supported");
                Ok(Default::default())
            }
            Support::Supported(language) => {
                debug!("extracting snippets of language: {language}");
                let content = std::fs::read(path).map_err(Error::ReadFile)?;
                match language {
                    Language::C => c99_tc3::Extractor::extract(opts, &content)?
                        .into_iter()
                        .map(|snippet| Self::from(path, &content, snippet))
                        .collect::<HashSet<_>>()
                        .pipe(Ok),
                }
            }
        }
    }
}

/// A deterministic representation of source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From)]
pub struct Fingerprint(Vec<u8>);

impl Fingerprint {
    /// Read the fingerprint as a byte vec.
    pub fn to_vec(self) -> Vec<u8> {
        self.0
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
/// Line end is inclusive: if a snippet starts on line 1 and ends before
/// the end of the line, `line_end` is line 1 as well.
///
/// Column end is inclusive: when displaying the snippet, the column
/// _indicated by_ this number should be included.
///
/// For example, if the full text was "hello world!",
/// and the snippet starts on byte 6 and ends on byte 10,
/// the returned `TextLocation` looks like:
/// ```ignore
/// TextLocation {
///   line_start: 1,
///   line_end: 1,
///   col_start: 7,
///   col_end: 11,
/// }
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, TypedBuilder)]
struct TextLocation {
    /// The line number on which the snippet starts.
    line_start: usize,

    /// The line number on which the snippet ends, inclusive.
    line_end: usize,

    /// The column at which the snippet starts on the line specified by `line_start`.
    col_start: usize,

    /// The column at which the snippet ends on the line specified by `line_end`, inclusive.
    col_end: usize,
}

impl TextLocation {
    /// Translate a [`Location`] in the given content into its textual indicators.
    fn new(loc: &snippets::Location, content: &[u8]) -> Self {
        let start_byte = loc.start_byte();
        let end_byte = loc.end_byte();
        let mut line_start = 1;
        let mut col_start = 1;

        const NEWLINE: u8 = b'\n';
        for b in content[..start_byte].iter().copied() {
            if b == NEWLINE {
                line_start += 1;
                col_start = 1;
            } else {
                col_start += 1;
            }
        }

        let mut line_end = line_start;
        let mut col_end = col_start;
        for b in content[start_byte..=end_byte].iter().copied() {
            if b == NEWLINE {
                line_end += 1;
                col_end = 1;
            } else {
                col_end += 1;
            }
        }

        Self {
            line_start,
            line_end,
            col_start,
            col_end,
        }
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use maplit::hashmap;
    use pretty_assertions::assert_eq;

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
        let expected = TextLocation::builder()
            .line_start(1)
            .line_end(1)
            .col_start(7)
            .col_end(11)
            .build();
        let got = TextLocation::new(&location, input);
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
        let expected = TextLocation::builder()
            .line_start(2)
            .line_end(3)
            .col_start(1)
            .col_end(11)
            .build();
        let got = TextLocation::new(&location, input);
        assert_eq!(got, expected);
    }
}

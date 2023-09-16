//! Communicates with the Millhone backend.

use std::{collections::HashSet, path::Path, time::Duration};

use base64::prelude::*;
use derive_more::From;
use getset::{CopyGetters, Getters};
use secrecy::{ExposeSecret, Secret};
use serde::{Deserialize, Serialize};
use snippets::{Language, Location};
use srclib::Locator;
use tap::{Conv, Pipe};
use typed_builder::TypedBuilder;
use ureq::{Agent, AgentBuilder, MiddlewareNext, Request};

pub mod types;
pub mod v1;

/// Describes a client for the Millhone service.
pub trait Client {
    /// Get the current service health.
    fn health(&self) -> Result<types::Health, types::Error>;

    /// Store a set of snippets.
    fn add_snippets(&self, snippets: HashSet<Snippet>) -> Result<(), types::Error>;
}

/// Provides types commonly used in the API module.
///
/// Many types are prefixed with `Api` when imported via the prelude to reduce collision
/// and to improve readability. Not _all_ API symbols are necessarily included;
/// the intention of the prelude convention is to provide symbols that are
/// _commonly_ referenced by a module.
///
/// If you're using the API it may be helpful to import all members of this:
/// ```no_run
/// use millhone::api::prelude::*;
/// ```
pub mod prelude {
    pub use crate::url::BaseUrl;

    pub use super::v1::Client as ApiClientV1;
    pub use super::Client as ApiClient;

    pub use super::types::Dependency as ApiDependency;
    pub use super::types::Error as ApiError;
    pub use super::types::Health as ApiHealth;
    pub use super::types::State as ApiState;
    pub use super::types::TransportError as ApiTransportError;
    pub use super::types::TransportErrorKind as ApiTransportErrorKind;
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
    pub fingerprint: Fingerprint,

    /// The full locator of the project from which this locator was extracted.
    #[getset(get = "pub")]
    pub locator: Locator,

    /// The target in the source code that was found and used to extract this snippet.
    #[getset(get = "pub")]
    pub target: String,

    /// The kind of snippet this represents.
    #[getset(get = "pub")]
    pub kind: String,

    /// The method used to normalize the snippet.
    #[getset(get = "pub")]
    pub method: String,

    /// The file path, relative to a project's root, which contained this snippet.
    #[getset(get = "pub")]
    pub file_path: String,

    /// The byte index at which the snippet starts in the code file.
    #[getset(get_copy = "pub")]
    pub byte_start: i64,

    /// The byte index at which the snippet ends in the code file.
    #[getset(get_copy = "pub")]
    pub byte_end: i64,

    /// The line index at which the snippet starts in the code file.
    #[getset(get_copy = "pub")]
    pub line_start: i32,

    /// The line index at which the snippet ends in the code file.
    #[getset(get_copy = "pub")]
    pub line_end: i32,

    /// The col index at which the snippet starts in the code file.
    #[getset(get_copy = "pub")]
    pub col_start: i32,

    /// The col index at which the snippet ends in the code file.
    #[getset(get_copy = "pub")]
    pub col_end: i32,

    /// The detected code language for the snippet.
    #[getset(get = "pub")]
    pub language: String,

    /// The ID of the ingestion run.
    /// This ID only means anything in the context of the ingesting application.
    #[getset(get = "pub")]
    pub ingest_id: String,
}

impl Snippet {
    /// Create a new snippet from an extracted snippet.
    pub fn from<L: Language>(
        ingest_id: &str,
        locator: &Locator,
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
            .locator(locator.to_owned())
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
            .ingest_id(ingest_id.to_string())
            .build()
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

impl Serialize for Fingerprint {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let encoded = BASE64_STANDARD.encode(&self.0);
        serializer.serialize_str(&encoded)
    }
}

impl<'de> Deserialize<'de> for Fingerprint {
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

fn build_default_agent(creds: Option<Credentials>) -> Agent {
    let app_name = env!("CARGO_PKG_NAME");
    let app_version = env!("CARGO_PKG_VERSION");

    let mut builder = AgentBuilder::new()
        // The intention is to provide the service with the name and version
        // so that we can track deployed app versions over time.
        .user_agent(&format!("{app_name}/{app_version}"))
        // Not based on anything specific but seems reasonable.
        .timeout(Duration::from_secs(30));

    // Inject authorization into every request here so that each method doesn't have to remember to.
    if let Some(creds) = creds {
        let credentials_header = creds.as_basic();
        builder = builder.middleware(move |req: Request, next: MiddlewareNext<'_>| {
            next.handle(req.set("Authorization", &credentials_header))
        })
    }

    builder.build()
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
    fn new(loc: &Location, content: &[u8]) -> Self {
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

/// API credential provided via [`HEADER_AUTHORIZATION`] on requests.
#[derive(Debug, Clone, Getters, TypedBuilder)]
#[getset(get = "pub")]
pub struct Credentials {
    /// Roughly equivalent to a "user" identity, but can also belong to an application.
    #[builder(setter(into))]
    key_id: String,

    /// Roughly equivalent to a "password" for a "user".
    /// Multiple secrets may be valid for the same key.
    ///
    /// The intention is to allow for revocation of specific secrets
    /// without having to revoke the whole key.
    #[builder(setter(transform = |secret: impl Into<String>| secret.conv::<String>().into() ))]
    secret: Secret<String>,
}

impl Credentials {
    /// Construct a new instance. If `secret` isn't already a secret, prefer `Credentials::build()`.
    pub fn new(key_id: String, secret: Secret<String>) -> Self {
        Self { key_id, secret }
    }

    /// Create an `Authorization: Basic` header value.
    pub fn as_basic(&self) -> String {
        let plain = format!("{}:{}", self.key_id, self.secret.expose_secret());
        let encoded = BASE64_STANDARD.encode(plain);
        format!("Basic {encoded}")
    }
}

impl Eq for Credentials {}
impl PartialEq for Credentials {
    fn eq(&self, other: &Self) -> bool {
        self.key_id == other.key_id && self.secret.expose_secret() == other.secret.expose_secret()
    }
}

impl std::fmt::Display for Credentials {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.key_id)
    }
}

#[cfg(test)]
mod tests {

    use snippets::Location;

    use crate::collection;

    use super::{types::*, TextLocation};

    #[test]
    fn health_overall() {
        let health: Health = collection! { "a" => State::Healthy, "b" => State::Healthy };
        assert_eq!(health.overall(), State::Healthy);

        let health: Health = collection! { "a" => State::Healthy, "b" => State::Degraded };
        assert_eq!(health.overall(), State::Degraded);

        let health: Health = collection! { "a" => State::Down, "b" => State::Degraded };
        assert_eq!(health.overall(), State::Down);
    }

    #[test]
    fn text_location_simple() {
        let input = b"hello world";
        //            ^     ^   ^
        // columns:   1     7   11
        let location = Location::from(6..10);
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
        let location = Location::from(6..22);
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

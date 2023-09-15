//! Types for the `ingest` subcommand.

use std::path::PathBuf;

use clap::{Parser, ValueEnum};
use getset::Getters;
use secrecy::Secret;
use srclib::Locator;
use strum::Display;

/// Options for snippet ingestion.
#[derive(Debug, Parser, Getters)]
#[getset(get = "pub")]
#[clap(version)]
pub struct Options {
    /// Provide the locator to which this snippet should be ingested.
    /// Note that this must be a full locator (including revision).
    #[clap(long, value_parser = Locator::parse)]
    locator: Locator,

    /// Provide the API Key ID for authentication.
    #[clap(long)]
    api_key_id: String,

    /// Provide the API Secret for authentication.
    #[clap(long)]
    api_secret: Secret<String>,

    /// Provide the ingest ID for the ingestion.
    /// If not provided, defaults to a new UUID.
    #[clap(long)]
    ingest_id: Option<String>,

    /// Ingest this combination of targets.
    /// Specify the argument multiple times to indicate additional options.
    #[clap(long = "target", default_value = "function")]
    targets: Vec<Target>,

    /// Ingest this combination of kinds.
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
    ///
    /// ```ignore
    /// fn say_happy_birthday(age: usize) -> String            // <- included
    /// {                                                      // <- omitted
    ///   println!("Happy birthday! You're {age} years old!"); // <- omitted
    /// }                                                      // <- omitted
    /// ```
    Signature,

    /// The body of the snippet.
    ///
    /// ```ignore
    /// fn say_happy_birthday(age: usize) -> String {          // <- omitted
    ///   println!("Happy birthday! You're {age} years old!"); // <- included
    /// }                                                      // <- omitted
    /// ```
    Body,

    /// Both signature and body in one snippet.
    ///
    /// ```ignore
    /// fn say_happy_birthday(age: usize) -> String {          // <- included
    ///   println!("Happy birthday! You're {age} years old!"); // <- included
    /// }                                                      // <- included
    /// ```
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
    /// Equivalent to [`Transform::Comment`] followed by [`Transform::Space`].
    ///
    /// # Example
    ///
    /// The original input:
    /// ```ignore
    /// fn say_happy_birthday(age: usize) -> String {
    ///   // TODO: make 'years' smart plural.
    ///   println!("Happy birthday! You're {age} years old!");
    /// }
    /// ```
    ///
    /// Is normalized to this:
    /// ```ignore
    /// fn say_happy_birthday(age: usize) -> String { println!("Happy birthday! You're {age} years old!"); }
    /// ```
    Code,

    /// Generated with any comments removed. Exactly what constitutes a comment is up to the implementation
    /// of the [`Extractor`] for the language being analyzed.
    ///
    /// # Example
    ///
    /// The original input:
    /// ```ignore
    /// fn say_happy_birthday(age: usize) -> String {
    ///   // TODO: make 'years' smart plural.
    ///   println!("Happy birthday! You're {age} years old!");
    /// }
    /// ```
    ///
    /// Is normalized to this:
    /// ```ignore
    /// fn say_happy_birthday(age: usize) -> String {
    ///   println!("Happy birthday! You're {age} years old!");
    /// }
    /// ```
    Comment,

    /// Generated with any whitespace characters (including newlines) normalized to a single space.
    /// Contiguous spaces are also collapsed to a single space. The specific test for whether
    /// a character is considered "whitespace" is the Unicode property `White_Space=yes`.
    ///
    /// # Example
    ///
    /// The original input:
    /// ```ignore
    /// fn say_happy_birthday(age: usize) -> String {
    ///   // TODO: make 'years' smart plural.
    ///   println!("Happy birthday! You're {age} years old!");
    /// }
    /// ```
    ///
    /// Is normalized to this:
    /// ```ignore
    /// fn say_happy_birthday(age: usize) -> String { // TODO: make 'years' smart plural. println!("Happy birthday! You're {age} years old!"); }
    /// ```
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

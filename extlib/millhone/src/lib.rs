//! The library for the Millhone CLI.
//!
//! Unlike most cases where a library exists in a crate that is also a binary,
//! this library is actually intended to be feasible to use as a library in another program.
//! This is because we want to keep the optionality open for FOSSA CLI or other programs
//! to use this library directly in the future rather than having to shell out to the CLI.
//!
//! # Versions
//!
//! The version of the crate represents the compatibility contract of this library,
//! rather than the versioning that end-user applications usually get.
//!
//! # Features
//!
//! The default features for this crate assume that it is building a binary.
//! To use as a library, ensure you specify `default-features = false` in your
//! Cargo.toml, otherwise dependencies such as `clap` will be included in your build.
//!
//! - `binary`: enables dependencies used when building as a binary. Enabled by default.

#![deny(clippy::unwrap_used)]
#![deny(unsafe_code)]
#![deny(missing_docs)]
#![warn(rust_2018_idioms)]

use tracing::warn;

mod ext;

pub mod api;
pub mod extract;
pub mod url;

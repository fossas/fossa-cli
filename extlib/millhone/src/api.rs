//! Communicates with the Millhone backend.

use std::{collections::HashSet, time::Duration};

use base64::prelude::*;
use getset::Getters;
use reqwest::{
    header::{HeaderMap, HeaderValue, AUTHORIZATION},
    ClientBuilder,
};
use secrecy::{ExposeSecret, Secret};
use stable_eyre::{eyre::Context, Report};
use tap::{Conv, TapFallible};
use typed_builder::TypedBuilder;
use ureq::{Agent, AgentBuilder, MiddlewareNext, Request};

use crate::extract::Fingerprint;

pub mod types;
pub mod v1;

pub use types::*;

/// Describes a client for the Millhone service.
#[async_trait::async_trait]
pub trait Client {
    /// Get the current service health.
    async fn health(&self) -> Result<Health, Error>;

    /// Store a set of snippets.
    fn add_snippets(&self, snippets: HashSet<ApiSnippet>) -> Result<(), Error>;

    /// Lookup snippets with the same fingerprint.
    async fn lookup_snippets(&self, fp: &Fingerprint) -> Result<HashSet<ApiSnippet>, Error>;
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

fn build_default_client(creds: Option<Credentials>) -> Result<reqwest::Client, Report> {
    let app_name = env!("CARGO_PKG_NAME");
    let app_version = env!("CARGO_PKG_VERSION");

    let mut builder = ClientBuilder::new()
        // The intention is to provide the service with the name and version
        // so that we can track deployed app versions over time.
        .user_agent(format!("{app_name}/{app_version}"))
        // Not based on anything specific but seems reasonable.
        .timeout(Duration::from_secs(30));

    // Inject authorization into every request here so that each method doesn't have to remember to.
    if let Some(creds) = creds {
        let auth = sensitive_header_value(creds.as_basic()).context("set auth header")?;
        let headers = HeaderMap::from_iter([(AUTHORIZATION, auth)]);
        builder = builder.default_headers(headers);
    }

    builder.build().context("build client")
}

fn sensitive_header_value(value: impl AsRef<str>) -> Result<HeaderValue, Report> {
    HeaderValue::from_str(value.as_ref())
        .context("parse value as header")
        .tap_ok_mut(|value| value.set_sensitive(true))
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
    use crate::collection;

    use super::types::*;

    #[test]
    fn health_overall() {
        let health: Health = collection! { "a" => State::Healthy, "b" => State::Healthy };
        assert_eq!(health.overall(), State::Healthy);

        let health: Health = collection! { "a" => State::Healthy, "b" => State::Degraded };
        assert_eq!(health.overall(), State::Degraded);

        let health: Health = collection! { "a" => State::Down, "b" => State::Degraded };
        assert_eq!(health.overall(), State::Down);
    }
}

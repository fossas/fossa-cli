//! Communicates with the Millhone backend.

use std::time::Duration;

use ureq::{Agent, AgentBuilder};

pub mod types;
pub mod v1;

/// Describes a client for the Millhone service.
pub trait Client {
    /// Get the current service health.
    fn health(&self) -> Result<types::Health, types::Error>;
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

pub(self) fn build_default_agent() -> Agent {
    let app_name = env!("CARGO_PKG_NAME");
    let app_version = env!("CARGO_PKG_VERSION");
    AgentBuilder::new()
        // The intention is to provide the service with the name and version
        // so that we can track deployed app versions over time.
        .user_agent(&format!("{app_name}/{app_version}"))
        // Not based on anything specific but seems reasonable.
        .timeout(Duration::from_secs(30))
        .build()
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

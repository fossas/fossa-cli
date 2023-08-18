//! Communicates with the Millhone backend.

use std::time::Duration;

use ureq::{Agent, AgentBuilder};

use self::prelude::*;

pub mod types;
pub mod v1;

/// Describes a client for the Millhone service.
pub trait Client {
    /// Get the current service health.
    fn health(&self) -> Result<Health, Error>;
}

/// If you're using the API it may be helpful to import all members of this.
///
/// ```no_run
/// use millhone::api::prelude::*;
/// ```
pub mod prelude {
    pub use super::types::*;
    pub use super::Client as ApiClient;
}

pub(self) fn build_default_agent() -> Agent {
    AgentBuilder::new()
        .timeout(Duration::from_secs(30))
        .user_agent(&user_agent())
        .build()
}

fn user_agent() -> String {
    format!("{}/{}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"))
}

#[cfg(test)]
mod tests {

    use crate::collection;

    use super::*;

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

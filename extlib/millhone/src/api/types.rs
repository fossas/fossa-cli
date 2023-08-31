//! Concrete types for API communications, independent of API version.

mod error;
pub use error::*;

use std::collections::HashMap;

use derive_more::{AsRef, Display, From, Into};
use itertools::intersperse;
use serde::{Deserialize, Serialize};

/// A dependency specified by the Millhone service.
/// These are dependencies of the actual service, not dependencies reported to users for their projects.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Into, AsRef, Display, Serialize, Deserialize)]
pub struct Dependency(String);

impl Dependency {
    /// Construct a new dependency.
    fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }
}

/// The current service health.
#[derive(Debug, Clone, PartialEq, Eq, From, Into, AsRef, Serialize, Deserialize)]
pub struct Health(HashMap<Dependency, State>);

impl std::fmt::Display for Health {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let deps = self.0.iter().map(|(dep, state)| format!("{dep}={state}"));
        let rendered = intersperse(deps, String::from(", ")).collect::<String>();
        f.write_str(&rendered)
    }
}

impl<S: Into<String>> FromIterator<(S, State)> for Health {
    fn from_iter<T: IntoIterator<Item = (S, State)>>(iter: T) -> Self {
        iter.into_iter()
            .map(|(dep, state)| (Dependency::new(dep), state))
            .collect::<HashMap<_, _>>()
            .into()
    }
}

impl Health {
    /// Report the overall state of the service.
    /// The service is considered unhealthy if any dependency is unhealthy.
    pub fn overall(&self) -> State {
        // Since `State` is ordered by severity, we can just return the highest severity state
        // as the overall state of the service.
        self.as_ref().values().cloned().max().unwrap_or_default()
    }
}

/// The current state of a given resource in the health check.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Serialize,
    Deserialize,
    Default,
    strum::Display,
)]
#[strum(serialize_all = "snake_case")]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum State {
    /// The resource is operating normally.
    #[default]
    Healthy,

    /// The resource is operating, but is not operating optimally.
    ///
    /// Not currently used, but would be ideal to have.
    Degraded,

    /// The resource is not responding.
    Down,
}

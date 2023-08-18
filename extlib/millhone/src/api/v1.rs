//! Communicates with v1 of the Millhone backend.

use ureq::Agent;

use crate::url::BaseUrl;

mod ping;

// v1 is expected to use types directly. Translation shouldn't be needed until/unless there's a v2.
pub(self) use super::types::*;

/// Implements the client for v1 of the Millhone API.
#[derive(Debug, Clone)]
pub struct Client {
    base_url: BaseUrl,
    agent: Agent,
}

impl Client {
    /// Construct a new client instance.
    pub fn new(base_url: &BaseUrl) -> Self {
        Self {
            base_url: base_url.to_owned(),
            agent: super::build_default_agent(),
        }
    }
}

impl super::Client for Client {
    #[tracing::instrument(skip(self))]
    fn health(&self) -> Result<Health, Error> {
        ping::run(&self.agent, &self.base_url)
    }
}

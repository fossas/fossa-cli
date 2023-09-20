//! Communicates with v1 of the Millhone backend.

use std::collections::HashSet;

use ureq::Agent;

use crate::{extract::Fingerprint, url::BaseUrl};

mod ingest;
mod lookup;
mod ping;

// v1 is expected to use types directly. Translation shouldn't be needed until/unless there's a v2.
use super::{types::*, Credentials};

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
            agent: super::build_default_agent(None),
        }
    }

    /// Construct a new client instance with credentials.
    pub fn authenticated(base_url: &BaseUrl, creds: Credentials) -> Self {
        Self {
            base_url: base_url.to_owned(),
            agent: super::build_default_agent(Some(creds)),
        }
    }
}

impl super::Client for Client {
    #[tracing::instrument(skip(self))]
    fn health(&self) -> Result<Health, Error> {
        ping::run(&self.agent, &self.base_url)
    }

    #[tracing::instrument(skip_all, fields(snippet_count = %snippets.len()))]
    fn add_snippets(&self, snippets: HashSet<super::ApiSnippet>) -> Result<(), Error> {
        ingest::run(&self.agent, &self.base_url, snippets)
    }

    #[tracing::instrument(skip_all, fields(fingerprint = %fp))]
    fn lookup_snippets(&self, fp: &Fingerprint) -> Result<HashSet<ApiSnippet>, Error> {
        lookup::run(&self.agent, &self.base_url, fp)
    }
}

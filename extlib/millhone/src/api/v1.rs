//! Communicates with v1 of the Millhone backend.

use std::collections::HashSet;

use stable_eyre::{eyre::Context, Report};
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
    client: reqwest::Client,
}

impl Client {
    /// Construct a new client instance.
    pub fn new(base_url: &BaseUrl) -> Result<Self, Report> {
        Ok(Self {
            base_url: base_url.to_owned(),
            agent: super::build_default_agent(None),
            client: super::build_default_client(None).context("configure default client")?,
        })
    }

    /// Construct a new client instance with credentials.
    pub fn authenticated(base_url: &BaseUrl, creds: Credentials) -> Result<Self, Report> {
        Ok(Self {
            base_url: base_url.to_owned(),
            agent: super::build_default_agent(Some(creds.clone())),
            client: super::build_default_client(Some(creds)).context("configure default client")?,
        })
    }
}

#[async_trait::async_trait]
impl super::Client for Client {
    #[tracing::instrument(skip(self))]
    async fn health(&self) -> Result<Health, Error> {
        ping::run(&self.client, &self.base_url).await
    }

    #[tracing::instrument(skip_all, fields(snippet_count = %snippets.len()))]
    fn add_snippets(&self, snippets: HashSet<super::ApiSnippet>) -> Result<(), Error> {
        ingest::run(&self.agent, &self.base_url, snippets)
    }

    #[tracing::instrument(skip_all, fields(fingerprint = %fp))]
    async fn lookup_snippets(&self, fp: &Fingerprint) -> Result<HashSet<ApiSnippet>, Error> {
        lookup::run(&self.client, &self.base_url, fp).await
    }
}

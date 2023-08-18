use ureq::{Agent, OrAnyStatus};
use url::Url;

use super::{Error, Health};
use crate::{ext::api::declare_route, url::BaseUrl};

declare_route!("api/v1/ping");

#[tracing::instrument(skip_all, fields(status, url))]
pub fn run(agent: &Agent, base: &BaseUrl) -> Result<Health, Error> {
    let target = route_url(base);
    tracing::Span::current().record("url", target.as_str());

    let response = agent.get(target.as_str()).call().or_any_status()?;
    tracing::Span::current().record("status", response.status());

    response.into_json().map_err(Error::ReadResponseBody)
}

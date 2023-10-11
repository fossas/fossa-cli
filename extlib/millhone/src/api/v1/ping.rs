use reqwest::Client;
use tap::TapFallible;

use url::Url;

use super::{Error, Health};
use crate::{ext::api::declare_route, url::BaseUrl};

declare_route!("api/v1/ping");

#[tracing::instrument(skip_all, fields(status, url))]
pub async fn run(agent: &Client, base: &BaseUrl) -> Result<Health, Error> {
    let target = route_url(base);
    tracing::Span::current().record("url", target.as_str());

    let response = agent
        .get(target)
        .send()
        .await
        .map_err(|err| Error::Request(route_url(base).to_string(), err))
        .tap_ok(|response| {
            tracing::Span::current().record("status", response.status().as_u16());
        })?;

    response
        .json()
        .await
        .map_err(|err| Error::ParseResponseBody(route_url(base).to_string(), err))
}

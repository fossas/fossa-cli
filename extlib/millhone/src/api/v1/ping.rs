use reqwest::Client;
use tap::TapFallible;

use url::Url;

use super::{Error, Health};
use crate::{ext::api::declare_route, url::BaseUrl};

declare_route!("api/v1/ping");

#[tracing::instrument(skip_all, fields(status, url))]
pub async fn run(agent: &Client, base: &BaseUrl) -> Result<Health, Error> {
    let url = route_url(base);
    let url = url.as_str();
    tracing::Span::current().record("url", url);

    let response = agent
        .get(url)
        .send()
        .await
        .map_err(|err| Error::Request(route_url(base).to_string(), err))
        .tap_ok(|response| {
            tracing::Span::current().record("status", response.status().as_u16());
        })?;

    let body = response
        .bytes()
        .await
        .map_err(|err| Error::DownloadResponseBody(url.to_string(), err))?;

    serde_json::from_slice(&body)
        .map_err(|err| Error::ParseResponseBody(url.to_string(), body.to_vec(), err))
}
